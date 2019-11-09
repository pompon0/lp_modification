module MGU(term'mgu,Valuation,empty,val'get) where

import HashSeq
import Ctx
import Pred
import qualified Control.Monad.Trans.State.Lazy as StateM
import qualified Control.Monad.Trans.Except as ExceptM
import Control.Monad
import Control.Monad.State.Class(get,modify)
import Control.Monad.Trans.Class(lift)
import qualified Data.Map as Map
import Debug.Trace
import Control.Lens hiding(assign)
import Control.Exception(assert)
import Text.Printf

-- Valuation is a function V-FV -> T[FV], represented by acyclic V-FV -> T[V] function
newtype Valuation = Valuation (Map.Map VarName Term) deriving(Eq)
empty = Valuation Map.empty

term'has :: Term -> VarName -> Bool
term'has t vn = any (==vn) (t^..term'varName'rec)

-- function T[V] -> T[FV], represented by the valuation
val'get :: Valuation -> Term -> Term
val'get v@(Valuation s) t = t & term'subst %~ (\vn -> case s^.at vn of {
  Nothing -> wrap $ TVar vn; Just t -> val'get v t})

val'set :: Valuation -> VarName -> Term -> Valuation
val'set v@(Valuation s) vn t = assert (not (term'has (val'get v t) vn) && (s^.at vn) == Nothing) $ Valuation (s & at vn ?~ t)

-------------------------------
--
type M = StateM.StateT Valuation (ExceptM.Except ())

throw :: M a
throw = lift $ ExceptM.throwE ()

-- TODO: early exit in case of identical terms (compared by hash)
-- Note that if you assign hashes to free variables and maintain the hashes for (V-FV)
-- (i.e. updating them on assign), mgu will process only the branches which actually end with a nontrivial assignment.
assign :: VarName -> Term -> M ()
assign xn t = do
  v <- get
  let t' = val'get v t
  when (t' /= wrap (TVar xn)) $ do
    when (term'has t' xn) throw
    StateM.put (val'set v xn t :: Valuation) :: M ()

term'mgu :: (Term,Term) -> Valuation -> Maybe Valuation
term'mgu lr s = case ExceptM.runExcept (StateM.runStateT (mgu lr) s) of
  Left () -> Nothing
  Right (_,s) -> Just s

mgu :: (Term,Term) -> M ()
mgu (x,y) = if x==y then return () else case (unwrap x, unwrap y) of
    (TFun f1 a1, TFun f2 a2) | f1 == f2 -> do { 
      when (length a1 /= length a2) (error $ printf "len(a1) = %d /= %d = len(a2) for %s" (length a1) (length a2) (show f1));
      mapM_ mgu (zip a1 a2);
    }
    -- TODO: you can implement path compression here
    (TFun _ _, TVar _) -> mgu (y,x)
    (TVar xn, _) -> do
      v <- get
      case unwrap (val'get v x) of
        TVar xn' -> assign xn' y
        x' -> mgu (wrap x',y)
    _ -> throw


