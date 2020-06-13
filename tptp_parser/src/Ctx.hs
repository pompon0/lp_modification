{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Ctx where

import HashSeq
import Control.Lens
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.List as List
import Text.Printf

import Prelude hiding(fail)
import Control.Monad hiding(fail)
import Control.Monad.Fail
import Control.Exception
import Debug.Trace

-------------------------------------

newtype Err a = Err (Either String a) deriving(Functor,Applicative,Monad)
instance MonadFail Err where { fail = Err . Left }

-------------------------------------

instance HashSeq String where hashSeq = map (unit.fromIntegral.fromEnum)
data Stack a = Stack'Empty | Stack { stack'top :: a, stack'values_ :: Set.Set a, stack'pop :: Stack a }


instance Stackable a => Eq (Stack a) where x == y = (x^.stack'ids) == (y^.stack'ids)
instance Stackable a => Show (Stack a) where show x = show (x^.stack'ids)

---------------------------------------------------

class (Show a, Eq a, Ord a) => Stackable a where
  stack'empty :: Stack a
  stack'empty = Stack'Empty

  stack'values :: Stack a -> Set.Set a
  stack'values s = case s of
    Stack'Empty -> Set.empty
    Stack _ v _ -> v 

  stack'push :: a -> Stack a -> Stack a
  stack'push h s = Stack h (Set.insert h (stack'values s)) s

  stack'has :: a -> Stack a -> Bool
  stack'has l s = Set.member l (stack'values s)
  
  stack'ids :: Iso' (Stack a) [a]
  stack'ids = iso to from where
    to Stack'Empty = []
    to (Stack h _ t) = h:to t
    from [] = Stack'Empty
    from (h:t) = stack'push h (from t)

  stack'push'unused :: (Int -> a) -> Stack a -> (a, Stack a)
  stack'push'unused f s = (x, stack'push x s) where
    x = fromJust $ f <$> List.find (\i -> not $ stack'has (f i) s) [0..]

  stack'push' :: [a] -> Stack a -> Stack a
  stack'push' ids s = foldr stack'push s ids

  stack'make :: Set.Set a -> Stack a
  stack'make labels = stack'push' (Set.toList labels) Stack'Empty

  pop :: Stack a -> (a,Stack a)
  pop s = (stack'top s, stack'pop s)

  stack'find :: Stack a -> a -> a
  stack'find s l = assert (stack'has l s) l

instance (Show a, Eq a, Ord a) => Stackable a
---------------------------------------------

newtype VarName = VarName Node deriving(Eq,Ord,HashSeq,Show)
newtype FunName = FunName Node deriving(Eq,Ord,HashSeq,Show)
newtype PredName = PredName Node deriving(Eq,Ord,HashSeq,Show)

eqPredName = PredName "eq"
extraConstName = FunName "c"

data Global = Global {
  _funs :: Stack FunName,
  _preds :: Stack PredName
} deriving(Eq,Show)
makeLenses ''Global

global'empty = Global Stack'Empty Stack'Empty

data GlobalVar = GlobalVar {
  _global' :: Global,
  _existsVars :: Stack VarName
} deriving(Eq,Show)
makeLenses ''GlobalVar
globalVar'empty = GlobalVar global'empty Stack'Empty

lambda :: (VarName -> x -> x) -> Stack VarName -> (Stack VarName,x) -> x
lambda w e sx = it sx where
  it (s,x) = if s==e then x else it (s',w vn x) where
    (vn,s') = pop s

