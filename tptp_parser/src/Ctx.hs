{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ctx where

import Control.Lens
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.List as List
import Text.Printf

import Prelude hiding(fail)
import Control.Monad hiding(fail)
import Control.Monad.Fail

-------------------------------------

newtype Err a = Err (Either String a) deriving(Functor,Applicative,Monad)
instance MonadFail Err where { fail = Err . Left }

-------------------------------------

data ID = ID { _uuid :: Int, _label :: String } deriving(Eq,Ord)
makeLenses ''ID

instance Show ID where { show (ID i l) = printf "%s_%d" l i }

data Stack a = Stack {
  _values :: Set.Set Int,
  _order :: [ID]
} deriving(Eq,Show)
makeLenses ''Stack

empty'Stack = Stack Set.empty []

stack'ids :: (Wrapped a, Unwrapped a ~ ID) => Stack a -> [a]
stack'ids s = s^..order.traverse._Unwrapped'

labels'stack :: (Wrapped a, Unwrapped a ~ ID) => [String] -> Stack a
labels'stack labels = Stack (Set.fromList $ map _uuid ids) ids where
  ids = map (uncurry ID) (zip [0..] labels)

labels'ids :: (Wrapped a, Unwrapped a ~ ID) => [String] -> [a]
labels'ids = stack'ids.labels'stack

{-make'Stack :: (Wrapped a, Unwrapped a ~ ID) => Set.Set String -> Stack a
make'Stack labels = Stack values order where
  order = map (uncurry ID) (zip [0..] (Set.toList labels))
  values = Set.fromList (map _uuid order)
-}

nextUuid :: Stack a -> Int
nextUuid s = s^.values.to Set.lookupMax.non (-1).to (+1)

push1 :: (Wrapped a, Unwrapped a ~ ID) => String -> Stack a -> (a,Stack a)
push1 label s = (x^._Unwrapped', s & order %~ (x:) & values %~ Set.insert (x^.uuid)) where
  x = ID (nextUuid s) label

push :: (Wrapped a, Unwrapped a ~ ID) => Set.Set String -> Stack a -> Stack a
push labels s = foldl (\s x -> s & order %~ (x:) & values %~ Set.insert (x^.uuid)) s ids where
  ids = map (uncurry ID) (zip [(nextUuid s)..] (Set.toList labels))

reloc :: (Wrapped a, Unwrapped a ~ ID) => a -> Stack a -> (a,Stack a)
reloc y = push1 (y^._Wrapped'.label) 

pop :: (Wrapped a, Unwrapped a ~ ID) => Stack a -> (a,Stack a)
pop s = (xl^._Unwrapped', s & values %~ Set.delete (xl^.uuid) & order .~ t) where
  (xl:t) = s^.order

find :: (Wrapped a, Unwrapped a ~ ID) => String -> Stack a -> a
find l s = List.find (^.label.to (==l)) (s^.order) ^.to fromJust._Unwrapped'

---------------------------------------------

newtype VarName = VarName ID deriving(Eq,Ord,Show)
newtype FunName = FunName ID deriving(Eq,Ord,Show)
newtype PredName = PredName ID deriving(Eq,Ord,Show)
makeWrapped ''VarName
makeWrapped ''FunName
makeWrapped ''PredName

eqPredName = PredName (ID (-1) "eq") :: PredName
extraConstName = FunName (ID (-1) "c") :: FunName

data Global = Global {
  _funs :: Stack FunName,
  _preds :: Stack PredName
} deriving(Eq,Show)
makeLenses ''Global

empty'Global = Global empty'Stack empty'Stack

data GlobalVar = GlobalVar {
  _global' :: Global,
  _existsVars :: Stack VarName
} deriving(Eq,Show)
makeLenses ''GlobalVar
empty'GlobalVar = GlobalVar empty'Global empty'Stack

lambda :: (VarName -> x -> x) -> Stack VarName -> (Stack VarName,x) -> x
lambda w e sx = it sx where
  it (s,x) = if s==e then x else it (s',w vn x) where
    (vn,s') = pop s

