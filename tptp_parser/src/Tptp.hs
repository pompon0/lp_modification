{-# LANGUAGE OverloadedLabels #-}
module Tptp where

import TptpLens
import Parser

import qualified Text.Parsec as P
import qualified Proto.Tptp as T
import Control.Lens
import Control.Exception
import Data.Text.Lens
import Data.ProtoLens.Labels()
import qualified Data.Map as Map
import Data.Maybe
import Text.Printf
import Data.List(intercalate)

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

file'string :: T.File -> String
file'string f = mconcat (((++"\n").input'string) <$> f^. #input)

input'string :: T.Input -> String
input'string i = printf "%s(%s,%s,%s)." lang (i^. #name) role (formula'string $ formula'assert $ i^. #formula) where
  role = (Map.fromList role'string'list)^.at (i^. #role).non (error "invalid role")
  lang = (Map.fromList language'string'list)^.at (i^. #language).non (error "invalid language")

formula'assert :: T.Formula -> T.Formula'Formula
formula'assert f = f^. #maybe'formula.non (error "empty formula")

varName'assert :: String -> String
varName'assert vn = case P.runP Parser.variable () "" vn of { Left err -> error (show err); Right _ -> vn }

funName'assert :: String -> String
funName'assert fn = case P.runP Parser.lower_word () "" fn of { Left err -> error (show err); Right _ -> fn }

formula'string :: T.Formula'Formula -> String
formula'string f = case f of
  T.Formula'Pred' pred -> let args = term'string <$> pred^.pred'args in
    case pred^. #type' of
      T.Formula'Pred'CUSTOM -> printf "%s(%s)" (pred^.pred'name) (intercalate "," args)
      T.Formula'Pred'EQ -> printf "%s = %s" l r where [l,r] = args 
  T.Formula'Quant' quant -> printf "%s[%s]: (%s)"
    (case quant^. #type' of { T.Formula'Quant'FORALL -> "!"; T.Formula'Quant'EXISTS -> "?" })
    (intercalate "," $ quant^..quant'varName.to varName'assert)
    (formula'string $ formula'assert $ quant^. #sub)
  T.Formula'Op op -> let args = (printf "(%s)".formula'string.formula'assert) <$> op^. #args in
    case op^. #type' of
      T.Formula'Operator'TRUE -> "$true" where [] = args
      T.Formula'Operator'FALSE -> "$false" where [] = args
      T.Formula'Operator'NEG -> printf "~%s" arg where [arg] = args
      T.Formula'Operator'OR -> intercalate " | " args
      T.Formula'Operator'AND -> intercalate " & " args
      T.Formula'Operator'IFF -> intercalate " <=> " args
      T.Formula'Operator'IMPL -> intercalate " => " args
      T.Formula'Operator'XOR -> intercalate " <~> " args
      T.Formula'Operator'NOR -> intercalate " ~| " args
      T.Formula'Operator'NAND -> intercalate " ~& " args

term'string :: T.Term -> String
term'string term = case term^. #type' of
  T.Term'VAR -> varName'assert $ term^.term'name
  T.Term'EXP -> printf "%s(%s)" (funName'assert $ term^.term'name) (intercalate "," $ term'string <$> term^. #args)
