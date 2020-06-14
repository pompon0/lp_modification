{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Tptp where

import Err
import HashSeq

import Prelude hiding(fail,id)
import qualified Text.Parsec as P
import qualified Proto.Tptp as T
import Control.Lens hiding(has)
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

data Node = Node {
  _type_ :: T.Type,
  _id :: Int32,
  _arity :: Int32,
  _name :: Maybe String
-- TODO: fail == if any field is insonsistent
} deriving(Eq)
makeFieldsNoPrefix ''Node

defName :: Node -> String
defName n = case n^.name of
  Just x -> x
  Nothing -> case n^.type_ of
    T.TERM_VAR -> printf "V%d" (n^.id)
    T.TERM_FUN -> printf "f%d" (n^.id)
    T.PRED -> printf "p%d" (n^.id)
    T.PRED_EQ -> "eq"

instance Ord Node where
  compare a b = compare (a^.id) (b^.id)

instance Show Node where
  show = defName

instance HashSeq Node where
  hashSeq a = [unit (fromIntegral (a^.id))]

variadicArity = -1 :: Int32
customArity = -2 :: Int32

standardNodes :: [(T.Type,Int32)] = [
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
  (T.PRED_EQ,2)]

typeArities :: [(T.Type,Int32)] = standardNodes ++ [
  (T.PRED,customArity),
  (T.TERM_FUN,customArity),
  (T.TERM_VAR,0)]

type NodeIndex = Map.Map Int32 Node
data NodeTree = NodeTree {
  _root :: Node,
  _args :: [NodeTree]
}
makeFieldsNoPrefix ''NodeTree

stream'tree :: NodeIndex -> [Int32] -> Err (NodeTree,[Int32])
stream'tree idx (h:t) = do
  Just node <-r$ idx^.at h
  (arity:t) <-r$ case (node^.arity) of { -1->t; a->a:t }
  (args,t) <- ([],t) & for [1..arity] (\_ cont (_,t) -> do
    (a,t) <- stream'tree idx t
    (at,t) <- cont ([],t)
    r$ (a:at,t))
  r$ (NodeTree node args,t)

tree'stream :: NodeTree -> [Int32] -> [Int32]
tree'stream (NodeTree n args) s = runIdentity $ do
  s <- s & for args (\t cont s -> r.tree'stream t =<< cont s)
  r$ (n^.id):s

index'add :: Node -> NodeIndex -> Err NodeIndex
index'add n idx = idx & at (n^.id) (\mn' -> case mn' of
  Nothing -> r$ Just n
  Just n' -> if n'==n then r$ mn'
    else fail "inconsistent node")

tree'index :: NodeTree -> NodeIndex -> Err NodeIndex
tree'index (NodeTree n args) idx  = do
  idx <- idx & for args (\a cont idx -> cont =<< tree'index a idx)
  index'add n idx

has :: Ord k => k -> Map.Map k v -> Bool
has k m = case m^.at k of { Nothing -> False; Just _ -> True }

index'withStandard :: NodeIndex -> (NodeIndex,T.Type -> Node)
index'withStandard idx = runIdentity $ do
  m <- Map.empty & for idx (\n cont m ->
    cont (m & at (n^.type_) .~ Just n))
  (_,m,idx) <- (0::Int32,m,idx) & for standardNodes (\(t,a) cont (i,m,idx) -> do
    cont =<< if has t m then r (i,m,idx) else do
      i <- i & while (\i -> has i idx) (\i -> r$ i+1)
      n <-r$ Just Node {
        _type_ = t,
        _id = i,
        _arity = a,
        _name = Nothing}
      r$ (i, m&at t.~n, idx&at i.~n))
  r$ (idx, \t -> fromJust (m^.at t))

mergeNI :: NodeIndex -> NodeIndex -> Err NodeIndex
mergeNI a b = fail "unimplemented"

emptyNI = Map.empty

newNodeIndex :: T.File -> Err NodeIndex
newNodeIndex f = Map.empty & for (f^. #nodes) (\n cont s -> do
  i <-r$ n^. #id
  t <-r$ n^. #type'
  a <-r$ let a = (Map.fromList typeArities)^.at t.non (error "") in
    if a==customArity then n^. #arity else a :: Int32
  x <-r$ case n^. #name.unpacked of { "" -> Nothing; x -> Just x }
  q <-r$ Node { _type_ = t, _id = i, _arity = n^. #arity, _name = x }
  cont (s & at i %~ (\Nothing -> Just q)))

