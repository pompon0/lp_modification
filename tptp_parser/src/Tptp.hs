{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Tptp where

import Ctx(Err)

import Prelude hiding(fail,id)
import qualified Text.Parsec as P
import qualified Proto.Tptp as T
import Control.Lens
import Control.Exception
import Control.Monad.Fail
import Data.Text.Lens
import Data.ProtoLens(defMessage)
import Data.ProtoLens.Labels()
import qualified Data.Map.Strict as Map
import Data.Maybe
import Text.Printf
import Data.List(intercalate)
import Data.Int(Int32)

r :: Monad m => a -> m a
r = return

-- s <- s & for [...] $ \a cont s -> do ...
for :: Monad m => [a] -> (a -> (s -> m s) -> (s -> m s)) -> (s -> m s)
for [] f s = r$ s
for (a:at) f s = f a (for at f) s

-------------------------------------

data Node = Node {
  _type_ :: T.Type,
  _id :: Int32,
  _arity :: Int32,
  _name :: Maybe String
}
makeFieldsNoPrefix ''Node

instance Eq Node where
  a==b = (a^.id) == (b^.id)
instance Ord Node where
  compare a b == compare (a^.id) (b^.id)
instance Show Node where
  show = defName
instance HashSeq where
  hashSeq a = [unit (a^.id)]

variadicArity = -1
customArity = -2

typeArities :: [(T.Type,Int32)] = [
  (T.FORALL,2), 
  (T.EXISTS,2),
  (T.FORM_NEG,1),
  (T.FORM_OR,variadicArity),
  (T.FORM_AND,variadicArity),
  (T.FORM_TRUE,0),
  (T.FORM_FALSE,0),
  (T.FORM_IFF,variadicArity),
  (T.FORM_IMPL,variadicArity), 
  (T.FORM_XOR,variadicArity),
  (T.FORM_NOR,variadicArity),
  (T.FORM_NAND,variadicArity),
  (T.PRED_EQ,2),
  (T.PRED,customArity),
  (T.TERM_FUN,customArity),
  (T.TERM_VAR,0)]

type NodeIndex = Map.Map Int32 Node
data NodeTree = NodeTree {
  _root :: Node,
  _args :: [NodeTree]
}
makeFieldsNoPrefix ''NodeTree

orFail :: MonadFail m => String -> (a -> m a) -> (Maybe a -> m (Maybe a))
orFail msg f ma = case ma of
  Nothing -> fail msg
  Just a -> Just <$> f a

stream'tree :: NodeIndex -> [Int32] -> Err (NodeTree,[Int32])
stream'tree idx (h:t) = do
  Just node <-r$ idx^.at h
  (arity:t) <-r$ case (node^.arity) of { -1->t; a->a:t }
  (args,t) <- ([],t) & for [1..arity] (\_ cont (_,t) -> do
    (a,t) <- stream'tree idx t
    (at,t) <- cont ([],t)
    r$ (a:at,t))
  r$ (NodeTree node args,t)

mergeNI :: NodeIndex -> NodeIndex -> Err NodeIndex
mergeNI a b = fail "unimplemented"

emptyNI = Map.empty

newNodeIndex :: T.File -> Err NodeIndex
newNodeIndex f = Map.empty & for (f^. #nodes) (\n cont s -> do
  i <-r$ n^. #id
  t <-r$ n^. #type'
  a <-r$ case Map.fromList typeArities t of { customArity -> n^. #arity; a -> a }
  x <-r$ case n^. #name of { "" -> Nothing; x -> Just x }
  q <-r$ Node { _type_ = t, _id = i, _arity = n^. #arity, _name = x }
  cont (s & at i %~ (\Nothing -> Just q))

defName :: Node -> String
defName n = case n^.name of
  Just x -> x
  Nothing -> case n^.type_ of
    T.TERM_VAR -> printf "V%d" (n^.id)
    T.TERM_FUN -> printf "f%d" (n^.id)
    T.PRED -> printf "p%d" (n^.id)
    T.PRED_EQ -> "eq"
