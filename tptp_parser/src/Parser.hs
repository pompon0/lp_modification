{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Parser where

import Data.TPTP
import Data.TPTP.Parse.Text
import Data.TPTP.Parse.Combinators hiding(type_)
import Data.TPTP.Pretty
import qualified Data.Text as Text
import Data.List.NonEmpty(NonEmpty((:|)))
import Control.Lens
import Data.ProtoLens(defMessage)
import Data.ProtoLens.Labels()
import Data.Char(ord)
import Data.Text.Lens
import Data.Attoparsec.Text(parseOnly)

import Tptp
import Ctx
import qualified Proto.Tptp as T

prettyPrint :: T.File -> Err String
prettyPrint x = show.pretty <$> tptp'file x

tptp'file :: T.File -> Err TPTP 
tptp'file f = do
  idx <- newNodeIndex f
  us <- [] & for (f^. #input) (\i cont _ -> do
    u <- unit'input idx i
    ut <- cont []
    r$ u:ut)
  r$ TPTP us

unit'input :: NodeIndex -> T.Input -> Err Unit 
unit'input idx i = do
  (f,_) <- stream'tree idx (i^. #formula)
  f <- case i^. #language of {
    T.Input'CNF -> CNF <$> cnf'formula f;
    T.Input'FOF -> FOF <$> fof'formula f;
  }
  r$ Unit
    (text'unitName (i^. #name))
    (Formula (Standard $ role'role (i^. #role)) f) Nothing

role'role :: T.Input'Role -> Role
role'role r = case r of {
    T.Input'AXIOM -> Axiom;
    T.Input'HYPOTHESIS -> Hypothesis;
    T.Input'DEFINITION -> Definition;
    T.Input'ASSUMPTION -> Assumption;
    T.Input'LEMMA -> Lemma;
    T.Input'THEOREM -> Theorem;
    T.Input'COROLLARY -> Corollary;
    T.Input'CONJECTURE -> Conjecture;
    T.Input'NEGATED_CONJECTURE -> NegatedConjecture;
    T.Input'PLAIN -> Plain;
    T.Input'FI_DOMAIN -> FiDomain;
    T.Input'FI_PREDICATES -> FiPredicates;
    T.Input'UNKNOWN -> Unknown }

decl'role :: Lens' Declaration Role
decl'role g (Formula (Standard r) f) = (\r -> Formula (Standard r) f) <$> g r

decl'formula :: Lens' Declaration Formula
decl'formula g (Formula r f) = Formula r <$> g f

node'var :: Node -> Var
node'var n = Var (defName n^.packed)

atom'text :: Text.Text -> Atom
atom'text = Atom 

node'name :: Named s => Node -> Name s
node'name n = Defined (atom'text (defName n^.packed))

text'unitName :: Text.Text -> UnitName
text'unitName t = Left (atom'text t)

-------------------------------------------

connective :: T.StandardNode -> Connective
connective n = case n of { 
  T.FORM_AND -> Conjunction;
  T.FORM_OR -> Disjunction;
  T.FORM_IMPL -> Implication;
  T.FORM_IFF -> Equivalence;
  T.FORM_XOR -> ExclusiveOr;
  T.FORM_NAND -> NegatedConjunction;
  T.FORM_NOR -> NegatedDisjunction;
  T.FORM_RIMPL -> ReversedImplication;
}

sign'not :: Sign -> Sign
sign'not Positive = Negative
sign'not Negative = Positive

formula'literal :: NodeTree -> Err (Sign,Literal)
formula'literal nt@(NodeTree n args) = case n^.type_ of
  NS T.FORM_NEG -> do
    [a] <-r$ args
    (s,l) <- formula'literal a
    r$ (sign'not s,l)
  _ -> do
    l <- pred'literal nt
    r$ (Positive,l)

cnf'formula :: NodeTree -> Err Clause
cnf'formula nt@(NodeTree n args) = case formula'literal nt of
  Err (Right l) -> r$ Clause (l :| [])
  Err (Left _) -> case n^.type_ of
    NS T.FORM_OR -> do
      args <- [] & for args (\a cont _ -> do
        a <- formula'literal a
        at <- cont []
        r$ (a:at))
      r$ Clause $ case args of
        [] -> ((Positive, Predicate (Reserved (Standard Falsum)) []) :| [])
        (h:t) -> (h :| t)

fof'formula :: NodeTree -> Err UnsortedFirstOrder
fof'formula nt@(NodeTree n args) = case n^.type_ of
  NC _ -> Atomic <$> pred'literal nt
  NS T.FORALL -> let [NodeTree v [],f] = args in Quantified Forall ((node'var v, Unsorted ()) :| []) <$> (fof'formula f)
  NS T.EXISTS -> let [NodeTree v [],f] = args in Quantified Exists ((node'var v, Unsorted ()) :| []) <$> (fof'formula f)
  NS T.FORM_TRUE -> let [] = args in r$ Atomic (Predicate (Reserved (Standard Tautology)) [])
  NS T.FORM_FALSE -> let [] = args in r$ Atomic (Predicate (Reserved (Standard Falsum)) [])
  NS T.FORM_NEG -> let [a] = args in Negated <$> (fof'formula a)
  NS x -> let (a:at) = args in do
      a <- fof'formula a
      a & for at (\a cont at -> do
        a <- fof'formula a
        cont (Connected a (connective x) at))

pred'literal :: NodeTree -> Err Literal
pred'literal (NodeTree n args) = do
  args :: [Term] <- r$ term'term <$> args
  r$ case n^.type_ of
    NC T.Node'PRED -> Predicate (node'name n) args 
    NS T.PRED_EQ -> let [l,r] = args in Equality l Positive r 

term'term :: NodeTree -> Term
term'term (NodeTree n args) = runIdentity $ do
  args <- r$ term'term <$> args
  r$ case n^.type_ of
    NC T.Node'VAR -> Variable (node'var n)
    NC T.Node'FUN -> Function (node'name n) args
