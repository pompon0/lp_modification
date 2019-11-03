module NNF where

import Ctx
import Pred
import qualified FOF
import Control.Lens

data NNF = Forall VarName NNF
  | Exists VarName NNF
  | And [NNF]
  | Or [NNF]
  | Atom Bool Pred
  deriving(Eq)

instance Show NNF where { show = show.nnf'fof }

nnf'pred :: Traversal' NNF Pred
nnf'pred f x = case x of
  Forall vn x -> Forall vn <$> nnf'pred f x
  Exists vn x -> Exists vn <$> nnf'pred f x
  And x -> And <$> (traverse.nnf'pred) f x
  Or x -> Or <$> (traverse.nnf'pred) f x
  Atom s p -> Atom s <$> f p

nnf'fof :: NNF -> FOF.FOF
nnf'fof nnf = case nnf of
  Forall vn x -> FOF.Forall vn (nnf'fof x)
  Exists vn x -> FOF.Exists vn (nnf'fof x)
  And x -> FOF.And (map nnf'fof x)
  Or x -> FOF.Or (map nnf'fof x)
  Atom True p -> FOF.Atom p
  Atom False p -> FOF.Neg (FOF.Atom p)

fof'nnf :: FOF.FOF -> NNF
fof'nnf f = _nnf True f where
  _nnf pos f = case f of
    FOF.Forall vn x -> (if pos then Forall else Exists) vn (_nnf pos x)
    FOF.Exists vn x -> (if pos then Exists else Forall) vn (_nnf pos x)
    FOF.And x -> (if pos then And else Or) (map (_nnf pos) x)
    FOF.Or x -> (if pos then Or else And) (map (_nnf pos) x)
    -- xor elimination allows to pull out the quantifiers
    -- NOTE: this reduction causes exponential blow up
    --       it can be optimized by caching
    FOF.Xor x y -> _nnf pos (FOF.Or [FOF.And [FOF.Neg x,y], FOF.And [x,FOF.Neg y]])
    FOF.Neg x -> _nnf (not pos) x
    FOF.Atom p -> Atom pos p
