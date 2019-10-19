{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
module Lib where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.List(intercalate,find)
import Data.List.Split(chunksOf)
import Control.Monad(join)

import Data.Monoid(Endo(..))
import Data.Functor.Const(Const(..))
import Data.Hashable(Hashable)
import Data.Ix(Ix)

import qualified Debug.Trace as Trace

import qualified Data.ProtoLens.TextFormat as TextFormat
import Data.ProtoLens.Message(Message)
import qualified Data.Text.Lazy as Text
import qualified Data.Set as Set
import qualified Data.Map as Map

import Control.Monad.IO.Class(MonadIO,liftIO)
import System.IO(hFlush,hPutStrLn,stdout,stderr)
import qualified Control.Monad.Trans.Except as ExceptM
import qualified Control.Monad.Trans.Reader as ReaderM
import Control.Lens(makeLenses,(&),at,(%~),(.~),(?~),(^.),(^?!),filtered,Index,IxValue,Lens',non,Ixed,At,view)

import qualified System.Clock as Clock

type Err a = ExceptM.Except String a

newtype FunName = FunName Int deriving (Eq,Num,Ord,Integral,Real,Enum,Hashable)
newtype PredName = PredName Int deriving(Eq,Num,Ord,Integral,Real,Enum,Hashable)
newtype VarName = VarName Int deriving (Eq,Num,Ord,Integral,Real,Enum,Hashable,Ix)

eqPredName = -1 :: PredName
redEQPredName = -2 :: PredName
redLTPredName = -3 :: PredName

extraConstName = -1 :: FunName

data Ctx = Ctx {
  _fromPredNames :: Map.Map PredName String,
  _fromFunNames :: Map.Map FunName String,
  _fromVarNames :: Map.Map VarName String
}
makeLenses ''Ctx

emptyCtx = Ctx {
  _fromPredNames = Map.fromList [(eqPredName,"eq")],
  _fromFunNames = Map.fromList [(extraConstName,"c")],
  _fromVarNames = Map.empty
}

class ShowCtx a where { showCtx :: a -> ReaderM.Reader Ctx String }
(!>) :: ShowCtx a => Ctx -> a -> String
(!>) ctx a = ReaderM.runReader (showCtx a) ctx

alloc :: (Num a, Eq a, Enum a, Ord a, Show a) => Map.Map a String -> (a, Map.Map a String)
alloc m = (x, m & at x ?~ show x) where
  x = [fromIntegral 0..] ^?! traverse.filtered (\i -> Maybe.isNothing (m^.at i))

at' :: At m => Index m -> Lens' m (IxValue m)
at' i f = at i (\(Just x) -> fmap Just (f x))

instance Show FunName where { show (FunName i) = "f" ++ show i }
instance Show PredName where { show (PredName i) = "p" ++ show i }
instance Show VarName where { show (VarName i) = "v" ++ show i }

instance ShowCtx FunName where { showCtx fn = view (fromFunNames.at' fn) }
instance ShowCtx PredName where { showCtx pn = view (fromPredNames.at' pn) }
instance ShowCtx VarName where { showCtx vn = view (fromVarNames.at' vn) }

-------------------------------------------

dPrint :: (Monad m, Show a) => a -> m ()
dPrint = Trace.traceShowM 

compose :: [a -> a] -> (a -> a)
compose = foldl (.) id 

select :: [a] -> [([a],a,[a])]
select [] = []
select (h:t) = ([],h,t) : map (\(l,x,r) -> (h:l,x,r)) (select t)

unique :: Ord a => [a] -> [a]
unique = Set.toAscList . Set.fromList

getUnique :: (Ord a, Num b) => a -> Map.Map a b -> (b,Map.Map a b)
getUnique k m = case Map.lookup k m of
  Just i -> (i,m)
  Nothing -> let i = fromIntegral (Map.size m) in (i, Map.insert k i m)

splitBy :: Ord k => (a -> k) -> [a] -> Map.Map k [a]
splitBy f [] = Map.empty
splitBy f (h:t) = Map.alter (\ml -> Just (h:(Maybe.fromMaybe [] ml))) (f h) (splitBy f t)

ix :: (Functor f, Num b, Ix b) => b -> (Maybe a -> f (Maybe a)) -> ([a] -> f [a])
ix i g [] = fmap (\_ -> []) (g Nothing)
ix 0 g (h:t) = fmap (\ma -> case ma of { Nothing -> (h:t); Just x -> (x:t)}) (g (Just h))
ix i g (h:t) = fmap (\la -> h:la) (ix (i-1) g t)

sepList :: ShowCtx a => [a] -> ReaderM.Reader Ctx String
sepList x = fmap (intercalate ",") (mapM showCtx x)

assert :: (Monad m, Show e) => Either e a -> m a
assert (Left err) = fail (show err)
assert (Right v) = return v

assertMaybe :: Monad m => Maybe a -> m a
assertMaybe Nothing = fail "Nothing"
assertMaybe (Just v) = return v

orFail :: Monad m => String -> Maybe a -> m a
orFail msg Nothing = fail msg
orFail _ (Just v) = return v

--------------------------------------

putStrLnE :: MonadIO m => String -> m ()
putStrLnE s = liftIO (hPutStrLn stderr s >> hFlush stderr)
printE :: (MonadIO m, Show a) => a -> m ()
printE x = putStrLnE (show x)

--------------------------------------

readProtoFile :: Message a => String -> IO a
readProtoFile path = readFile path >>= assert . TextFormat.readMessage . Text.pack 

