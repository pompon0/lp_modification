{-# LANGUAGE TemplateHaskell #-}
module Valid(counterExample) where

import DNF
import Pred
import Control.Monad(join)
import qualified Data.Set as Set
import Control.Lens
import Control.Monad.Trans.Except as ExceptM

neg :: Atom -> Atom
neg a = a & atom'sign %~ not 

data State = State { _state'orForm :: OrForm, _state'val :: Set.Set Atom }
makeLenses ''State

setVal :: Atom -> State -> State
setVal a s = s
  & state'val %~ Set.insert a
  & state'orForm.orForm'andClauses %~ filter (\c -> not (elem (neg a) $ c^.andClause'atoms))
  & state'orForm.orForm'andClauses.traverse.andClause'atoms %~ filter (/=a)

validRec :: [Pred] -> State -> ExceptM.Except (Set.Set Atom) ()
validRec preds s = case s^.state'orForm.orForm'andClauses of
    [] -> ExceptM.throwE (s^.state'val)
    _ | elem (AndClause []) (s^.state'orForm.orForm'andClauses) -> return ()
    _ -> case preds of
      [] -> error ""
      (h:t) -> do
        validRec t (setVal (Atom True h) s)
        validRec t (setVal (Atom False h) s)

counterExample :: OrForm -> Maybe (Set.Set Atom)
counterExample f = let
  preds = Set.fromList $ f^..orForm'andClauses.traverse.andClause'atoms.traverse.atom'pred
  in case ExceptM.runExcept (validRec (Set.toList preds) (State f Set.empty)) of
    Left e -> Just e
    Right () -> Nothing

