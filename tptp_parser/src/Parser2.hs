{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Parser2 where

import Data.TPTP
import Data.TPTP.Parse.Text
import Data.TPTP.Parse.Combinators
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

parse :: String -> Err T.File
parse input = do 
  x <- Err $ parseOnly tptp (Text.strip $ input^.packed)
  return $ x^.tptp'file

prettyPrint :: T.File -> String
prettyPrint x = show $ pretty $ x^.from tptp'file

tptp'file :: Iso' TPTP T.File
tptp'file = iso
  (\(TPTP units) -> defMessage & #input .~ (units^..traverse.unit'input))
  (\file -> TPTP (file^.. #input.traverse.from unit'input))

unit'input :: Iso' Unit T.Input
unit'input = iso
  (\(Unit name decl _) -> defMessage
      & #language .~ (decl^.decl'formula.formula'language)
      & #name .~ (name^.unitName'text)
      & #role .~ (decl^.decl'role.role'role)
      & #formula. #maybe'formula ?~ (decl^.decl'formula.formula'formula)
  )
  (\i -> 
    let f = case i^. #language of {
      T.Input'CNF -> CNF (i^. #formula.formula'assert.from cnf'formula);
      T.Input'FOF -> FOF (i^. #formula.formula'assert.from fof'formula);
    } in Unit
      (i^. #name.from unitName'text)
      (Formula (Standard $ i^. #role.from role'role) f) Nothing
  )

role'role :: Iso' Role T.Input'Role
role'role = iso
  (\r -> case r of {
    Axiom -> T.Input'AXIOM;
    Hypothesis -> T.Input'HYPOTHESIS;
    Definition -> T.Input'DEFINITION;
    Assumption -> T.Input'ASSUMPTION;
    Lemma -> T.Input'LEMMA;
    Theorem -> T.Input'THEOREM;
    Corollary -> T.Input'COROLLARY;
    Conjecture -> T.Input'CONJECTURE;
    NegatedConjecture -> T.Input'NEGATED_CONJECTURE;
    Plain -> T.Input'PLAIN;
    FiDomain -> T.Input'FI_DOMAIN;
    FiPredicates -> T.Input'FI_PREDICATES;
    Unknown -> T.Input'UNKNOWN })
  (\r -> case r of {
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
    T.Input'UNKNOWN -> Unknown })

decl'role :: Lens' Declaration Role
decl'role g (Formula (Standard r) f) = (\r -> Formula (Standard r) f) <$> g r

decl'formula :: Lens' Declaration Formula
decl'formula g (Formula r f) = Formula r <$> g f

formula'language :: Getter Formula T.Input'Language
formula'language = to (\f -> case f of {
    CNF _ -> T.Input'CNF;
    FOF _ -> T.Input'FOF;
    TFF0 _ -> error "unsupported TFF0";
    TFF1 _ -> error "unsupported TFF1";
  })

formula'formula :: Getter Formula T.Formula'Formula
formula'formula = to (\f -> case f of {
    CNF cnf -> cnf^.cnf'formula;
    FOF fof -> fof^.fof'formula;
  })

var'text :: Iso' Var Text.Text
var'text = iso (\(Var t) -> t) Var

atom'text :: Iso' Atom Text.Text
atom'text = iso (\(Atom t) -> t) Atom 

name'text :: Named s => Iso' (Name s) Text.Text
name'text = iso (\n -> (show $ pretty n)^.packed) (\t -> Defined (t^.from atom'text))

unitName'text :: Iso' UnitName Text.Text
unitName'text = iso
  (\n -> case n of { Left l -> l^.atom'text; Right r -> show r^.packed })
  (\t -> Left (t^.from atom'text))

-------------------------------------------

operator :: T.Formula'Operator'Type -> [T.Formula'Formula] -> T.Formula'Formula
operator o args = T.Formula'Op $ defMessage & #type' .~ o & #args .~ (args^..traverse.from formula'assert)

neg :: T.Formula'Formula -> T.Formula'Formula
neg f = operator T.Formula'Operator'NEG [f]

sign :: Sign -> (T.Formula'Formula -> T.Formula'Formula)
sign s = case s of { Positive -> id; Negative -> neg }

sign'not :: Sign -> Sign
sign'not Positive = Negative
sign'not Negative = Positive

connective'operator :: Iso' Connective T.Formula'Operator'Type
connective'operator = iso
  (\c -> case c of {
    Conjunction -> T.Formula'Operator'AND;
    Disjunction -> T.Formula'Operator'OR;
    Implication -> T.Formula'Operator'IMPL;
    Equivalence -> T.Formula'Operator'IFF;
    ExclusiveOr -> T.Formula'Operator'XOR;
    NegatedConjunction -> T.Formula'Operator'NAND;
    NegatedDisjunction -> T.Formula'Operator'NOR;
    ReversedImplication -> T.Formula'Operator'RIMPL;
  })
  (\o -> case o of {
    T.Formula'Operator'AND -> Conjunction;
    T.Formula'Operator'OR -> Disjunction;
    T.Formula'Operator'IMPL -> Implication;
    T.Formula'Operator'IFF -> Equivalence;
    T.Formula'Operator'XOR -> ExclusiveOr;
    T.Formula'Operator'NAND -> NegatedConjunction;
    T.Formula'Operator'NOR -> NegatedDisjunction;
    T.Formula'Operator'RIMPL -> ReversedImplication;
  })

quantifier'quant ::Iso' Quantifier T.Formula'Quant'Type
quantifier'quant = iso
  (\q -> case q of {
    Forall -> T.Formula'Quant'FORALL;
    Exists -> T.Formula'Quant'EXISTS;
  })
  (\q -> case q of {
    T.Formula'Quant'FORALL -> Forall;
    T.Formula'Quant'EXISTS -> Exists;
  })

formula'literal :: T.Formula'Formula -> (Sign,Literal)
formula'literal f = case f of
  T.Formula'Op o -> case o^. #type' of
    T.Formula'Operator'NEG -> let
      [a] = o^. #args
      (s,l) = a^.formula'assert.to formula'literal
      in (sign'not s,l)
  T.Formula'Pred' _ -> (Positive, f^.from literal'pred)

cnf'formula :: Iso' Clause T.Formula'Formula
cnf'formula = iso
  (\(Clause (h :| t)) -> operator T.Formula'Operator'OR $ map (\(s,l) -> sign s (l^.literal'pred)) (h:t))
  (\f -> case f of {
    T.Formula'Pred' _ -> Clause ((Positive, f^.from literal'pred) :| []);
    T.Formula'Op o -> case (o^. #type') of {
      T.Formula'Operator'OR -> case o^.. #args.traverse.formula'assert.to formula'literal of {
        [] -> Clause ((Positive, Predicate (Reserved (Standard Falsum)) []) :| []);
        (h:t) -> Clause (h :| t);
      };
      _ -> Clause (formula'literal f :| []);
    };
  })
      

fof'formula :: Iso' UnsortedFirstOrder T.Formula'Formula
fof'formula = iso
  (\x -> case x of
    Atomic l -> l^.literal'pred
    Negated y -> neg (y^.fof'formula)
    Connected y c z -> operator (c^.connective'operator) [y^.fof'formula, z^.fof'formula]
    Quantified q (vh :| vt) f -> T.Formula'Quant' $ defMessage
      & #type' .~ (q^.quantifier'quant)
      & #var .~ ((vh:vt)^..traverse._1.var'text)
      & #sub .~ (f^.fof'formula.from formula'assert)
  )
  (\f -> case f of
    T.Formula'Pred' _ -> Atomic (f^.from literal'pred)
    T.Formula'Quant' q -> let (vh:vt) = q^.. #var.traverse.from var'text.to (\v -> (v,Unsorted ())) in
      Quantified (q^. #type'.from quantifier'quant) (vh :| vt) (q^. #sub.formula'assert.from fof'formula)
    T.Formula'Op o -> let args = (o^.. #args.traverse.formula'assert.from fof'formula) in
      case o^. #type' of {
        T.Formula'Operator'NEG -> let [a] = args in Negated a;
        T.Formula'Operator'TRUE -> let [] = args in Atomic (Predicate (Reserved (Standard Tautology)) []);
        T.Formula'Operator'FALSE -> let [] = args in Atomic (Predicate (Reserved (Standard Falsum)) []);
        _ -> let (ha:ta) = args in foldl (\l r -> Connected l (o^. #type'. from connective'operator) r) ha ta;
    }
  )


literal'pred :: Iso' Literal T.Formula'Formula
literal'pred = iso
  (\p -> case p of {
    Predicate n args -> case n of {
      Reserved (Standard Tautology) -> operator T.Formula'Operator'TRUE [];
      Reserved (Standard Falsum) -> operator T.Formula'Operator'FALSE [];
      Defined a -> T.Formula'Pred' $ defMessage 
        & #type' .~ T.Formula'Pred'CUSTOM
        & #name .~ (a^.atom'text)
        & #args .~ (args^..traverse.term'term);
    };
    Equality l s r -> sign s $ T.Formula'Pred' $ defMessage
      & #type' .~ T.Formula'Pred'EQ
      & #args .~ [l^.term'term,r^.term'term];
  } :: T.Formula'Formula)
  (\(T.Formula'Pred' p) -> let
    args = (p^.. #args.traverse. from term'term) in
    case p^. #type' of {
      T.Formula'Pred'CUSTOM -> Predicate (p^. #name.from name'text) args;
      T.Formula'Pred'EQ -> let [l,r] = args in Equality l Positive r;
  } :: Literal)

term'term :: Iso' Term T.Term
term'term = iso
  (\t -> case t of {
    Function n args -> defMessage
      & #type' .~ T.Term'EXP
      & #name .~ (n^.name'text)
      & #args .~ (args^..traverse.term'term);
    Variable v -> defMessage
      & #type' .~ T.Term'VAR
      & #name .~ (v^.var'text);
    Number n -> error "numbers unsupported";
    DistinctTerm _ -> error "distinct objects unsupported";
  })
  (\t -> case t^. #type' of
    T.Term'EXP -> Function
      (t^. #name.from name'text)
      (t^.. #args.traverse.from term'term)
    T.Term'VAR -> Variable
      (t^. #name.from var'text)
  )
