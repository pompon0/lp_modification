{-# LANGUAGE OverloadedLabels #-}
module TptpLens where

import qualified Proto.Tptp as T
import Control.Lens
import Data.Text.Lens
import Data.ProtoLens.Labels()

file'formula :: Traversal' T.File T.Formula'Formula
file'formula = #input. traverse. #formula. #maybe'formula. traverse

formula'pred :: Traversal' T.Formula'Formula T.Formula'Pred
formula'pred f x = case x of
  T.Formula'Pred' pred -> T.Formula'Pred' <$> f pred
  T.Formula'Quant' quant -> T.Formula'Quant' <$> (#sub. #maybe'formula.traverse.formula'pred) f quant
  T.Formula'Op op -> T.Formula'Op <$> (#args.traverse. #maybe'formula.traverse.formula'pred) f op

pred'name :: Traversal' T.Formula'Pred String
pred'name f x = case (x^. #type') of
  T.Formula'Pred'CUSTOM -> (#name.unpacked) f x
  T.Formula'Pred'EQ -> pure x

quant'varName :: Traversal' T.Formula'Quant String
quant'varName = #var.traverse.unpacked

quant'sub :: Traversal' T.Formula'Quant T.Formula'Formula
quant'sub = #sub. #maybe'formula.traverse

pred'args :: Lens' T.Formula'Pred [T.Term]
pred'args = #args

term'name :: Lens' T.Term String
term'name = #name.unpacked

term'varName'rec :: Traversal' T.Term String
term'varName'rec f x = case (x^. #type') of  
  T.Term'VAR -> term'name f x
  T.Term'EXP -> (#args.traverse.term'varName'rec) f x

term'funName'rec :: Traversal' T.Term String
term'funName'rec f x = case (x^. #type') of
  T.Term'VAR -> pure x
  T.Term'EXP -> pure (\n a -> x & term'name.~n & #args.~a)
    <*> f (x^.term'name)
    <*> (traverse.term'funName'rec) f (x^. #args)
