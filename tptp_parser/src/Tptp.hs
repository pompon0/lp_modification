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

role'string'list :: [(T.Input'Role,String)]
role'string'list = [
  (T.Input'AXIOM,"axiom"),
  (T.Input'HYPOTHESIS,"hypothesis"),
  (T.Input'DEFINITION,"definition"),
  (T.Input'ASSUMPTION,"assumption"),
  (T.Input'LEMMA,"lemma"),
  (T.Input'THEOREM,"theorem"),
  (T.Input'COROLLARY,"corollary"),
  (T.Input'CONJECTURE,"conjecture"),
  (T.Input'NEGATED_CONJECTURE,"negated_conjecture"),
  (T.Input'PLAIN,"plain"),
  (T.Input'TYPE,"type"),
  (T.Input'FI_DOMAIN,"fi_domain"),
  (T.Input'FI_FUNCTORS,"fi_functors"),
  (T.Input'FI_PREDICATES,"fi_predicates"),
  (T.Input'UNKNOWN,"unknown")]

language'string'list :: [(T.Input'Language,String)]
language'string'list = [
  (T.Input'FOF,"fof"),
  (T.Input'CNF,"cnf")]

data NodeType = NS T.StandardNode | NC T.Node'Type
data Node = Node {
  _type_ :: NodeType,
  _id :: Int32,
  _arity :: Int32,
  _name :: Maybe String
}
makeFieldsNoPrefix ''Node

standardArities :: [(T.StandardNode,Int32)] = [
  (T.FORALL,2), 
  (T.EXISTS,2),
  (T.FORM_NEG,1),
  (T.FORM_OR,-1),
  (T.FORM_AND,-1),
  (T.FORM_TRUE,0),
  (T.FORM_FALSE,0),
  (T.FORM_IFF,-1),
  (T.FORM_IMPL,-1), 
  (T.FORM_XOR,-1),
  (T.FORM_NOR,-1),
  (T.FORM_NAND,-1),
  (T.PRED_EQ,-1)]

standardNodes :: Map.Map Int32 Node
standardNodes = runIdentity $ Map.empty & for standardArities (\(t,a) cont m -> do
  id <- fromIntegral $ fromEnum t
  cont (m & at id %~ \Nothing -> Just $ Node {
    _type_ = NS t,
    _id = id,
    _arity = a,
    _name = Nothing
  }))

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
newNodeIndex f = do
  s <-r$ Map.empty
  s <- s & for (f^. #nodes) (\n cont s -> do
    id <-r$ n^. #id
    cont (s & at id %~ \Nothing -> Just $ Node {
      _type_ = NC (n^. #type'),
      _id = id,
      _arity = n^. #arity,
      _name = Nothing
    })) 
  s <- s & for (f^. #names) (\n cont s -> do
    id <-r$ n^. #id 
    x <-r$ n^. #name.unpacked
    s <- s & (at id.orFail "".name) (\Nothing -> r$ Just x)
    cont s)
  r$ standardNodes <> s

file'string :: T.File -> Err String
file'string f = do
  idx <- newNodeIndex f
  out <- "" & for (f^. #input) (\i cont s -> do
    is <- input'string idx i
    cont (s ++ is ++ "\n"))
  r$ out

input'string :: NodeIndex -> T.Input -> Err String
input'string idx i = do
  Just role <-r$ (Map.fromList role'string'list)^.at (i^. #role)
  Just lang <-r$ (Map.fromList language'string'list)^.at (i^. #language)
  (tree,[]) <- stream'tree idx (i^. #formula)
  r$ printf "%s(%s,%s,%s)." lang (i^. #name) role (formula'string tree)

defName :: Node -> String
defName n = case n^.name of
  Just x -> x
  Nothing -> case n^.type_ of
    NC T.Node'VAR -> printf "V%d" (n^.id)
    NC T.Node'FUN -> printf "f%d" (n^.id)
    NC T.Node'PRED -> printf "p%d" (n^.id)

formula'string :: NodeTree -> String
formula'string (NodeTree n args) = let
  join = intercalate
  in case n^.type_ of
    NS T.FORALL -> let { [NodeTree v [],f] = args; }
      in printf "![%s]: (%s)" (defName v) (formula'string f)
    NS T.EXISTS -> let { [NodeTree v [],f] = args; }
      in printf "?[%s]: (%s)" (defName v) (formula'string f)
    NS T.FORM_NEG -> let [f] = args in printf "~%s" (formula'string f)
    NS T.FORM_OR -> join " | " (formula'string <$> args)
    NS T.FORM_AND -> join " & " (formula'string <$> args)
    NS T.FORM_TRUE -> let [] = args in "$true"
    NS T.FORM_FALSE -> let [] = args in "$false"
    NS T.FORM_IFF -> join " <=> " (formula'string <$> args)
    NS T.FORM_IMPL -> join " => " (formula'string <$> args)
    NS T.FORM_XOR -> join " <~> " (formula'string <$> args)
    NS T.FORM_NOR -> join " ~| " (formula'string <$> args)
    NS T.FORM_NAND -> join " ~& " (formula'string <$> args)
    NS T.PRED_EQ -> let [a,b] = args in printf "%s = %s" (term'string a) (term'string b)
    NC T.Node'PRED -> printf "%s(%s)" (defName n) (join "," (term'string <$> args))
  
term'string :: NodeTree -> String
term'string (NodeTree n args) = case n^.type_ of
  NC T.Node'VAR -> defName n
  NC T.Node'FUN -> printf "%s(%s)" (defName n) (intercalate "," (term'string <$> args))
