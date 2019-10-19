module NNF(nnf,Form(..),Pred(..),Term(..)) where

import qualified Form as F
import Pred
import Lib
import qualified Control.Monad.Trans.Reader as ReaderM

data Form = Forall VarName Form
  | Exists VarName Form
  | And [Form]
  | Or [Form]
  | Atom Bool Pred
  deriving(Eq)

instance Show Form where
  show (Forall f) = "A " ++ show f
  show (Exists f) = "E " ++ show f
  show (And x) = "and(" ++ sepList x ++ ")"
  show (Or x) = "or(" ++ sepList x ++ ")"
  show (Atom True p) = show p
  show (Atom False p) = "-" ++ show p

nnf :: (Ctx,F.Form) -> (Ctx,Form)
nnf (ctx,f) = (ctx, _nnf False ctx f)
_nnf :: Bool -> Ctx -> F.Form -> Form
_nnf n ctx f = case f of
  F.Forall x -> (if n then Exists else Forall) (_nnf n ctx x)
  F.Exists x -> (if n then Forall else Exists) (_nnf n ctx x)
  F.And x -> (if n then Or else And) (map (_nnf n ctx) x)
  F.Or x -> (if n then And else Or) (map (_nnf n ctx) x)
  -- xor elimination allows to pull out the quantifiers
  -- NOTE: this reduction causes exponential blow up
  --       it can be optimized by caching
  F.Xor x y -> _nnf n ctx (F.Or [F.And [F.Neg x,y], F.And [x,F.Neg y]])
  F.Neg x -> _nnf (not n) ctx x
  F.Atom p -> Atom (not n) p
