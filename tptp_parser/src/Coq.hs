module Coq where

import Text.Printf

type Name = String
data Proof = Proof [Stmt]
data Stmt = Axiom Name Expr | Def Name Expr Expr

data Expr = Forall Name Expr Expr
  | Exists Name Expr Expr
  | And Expr Expr
  | Or Expr Expr
  | Not Expr
  | Fun Name Expr Expr
  | MatchAnd Expr Expr -- (x,y) (x -> y -> ?)
  | MatchOr Expr Expr Expr -- (x|y) (x -> ?) (y -> ?)
  | MatchExists Expr Expr -- (ex a p) ((x:a) -> p x -> ?)
  | Apply Expr Expr
  | Value Name

instance Show Proof where
  show (Proof s) = mconcat (map ((++"\n").show) s)
instance Show Stmt where
  show (Axiom n t) = printf "Axiom %s : %s." n (show t)
  show (Def n e t) = printf "Definition %s : %s := %s." n (show t) (show e)
instance Show Expr where
  show (Forall n t e) = printf "forall (%s : %s), %s" n (show t) (show e)
  show (Exists n t e) = printf "exists (%s : %s), %s" n (show t) (show e)
  show (And l r) = printf "(%s) /\\ (%s)" (show l) (show r)
  show (Or l r) = printf "(%s) \\/ (%s)" (show l) (show r)
  show (Not x) = printf "~(%s)" (show x)
  show (Fun n t e) = printf "fun (%s : %s) => %s" n (show t) (show e)
  show (MatchAnd a e) = printf "match (%s) with conj _x,_y => (%s) _x _y end" (show a) (show e)
  show (MatchOr o l r) = printf "match (%s) with or_introl _x => (%s) _x | or_intror _y => (%s) _y end" (show o) (show l) (show r)
  show (MatchExists e f) = printf "match (%s) with ex_intro _ _x _px => (%s) _x _px end" (show e) (show f)
  show (Apply f x) = printf "(%s) (%s)" (show f) (show x)
  show (Value n) = n

