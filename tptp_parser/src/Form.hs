{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Form where

import Pred
import Prelude hiding(fail)
import Control.Monad(join,when,fail)
import Control.Monad.Trans.Class(lift)
import Control.Lens
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.Trans.Except as ExceptM
import qualified Control.Monad.Trans.Reader as ReaderM
import qualified Control.Monad.Trans.State.Lazy as StateM
import qualified Data.Map as Map
import Data.ProtoLens.Labels()
import qualified Data.Text as Text
import qualified Data.List as List
import Data.Ix(Ix)
import qualified Data.Set as Set
import Data.Set((\\))
import Data.Maybe(fromMaybe,fromJust)
import Data.ProtoLens(defMessage)
import Text.Printf

import Lib
import qualified Proto.Tptp as T

data Form = Forall VarName Form
  | Exists VarName Form
  | And [Form]
  | Or [Form]
  | Xor Form Form
  | Neg Form
  | Atom Pred
  deriving(Eq)

instance Show (Ctx,Form) where
  show (ctx, Forall vn f) = printf "A[%s] %s" (fromJust $ ctx^.fromVarNames.at vn) (show (ctx,f))
  show (ctx, Exists vn f) = printf "E[%s] %s" (fromJust $ ctx^.fromVarNames.at vn) (show (ctx,f))
  show (ctx, And x) = printf "and(%s)" (sepList $ map (\f -> (ctx,f)) x)
  show (ctx, Or x) = printf "or(%s)" (sepList $ map (\f -> (ctx,f)) x)
  show (ctx, Xor l r) = printf "xor(%s,%s)" (show (ctx,l)) (show (ctx,r))
  show (ctx, Neg f) = printf "-%s" (show (ctx,f))
  show (ctx, Atom p) = show (ctx,p)

---------------------------------------

preds :: Form -> [Pred]
preds f = case f of
  Forall x -> preds x
  Exists x -> preds x
  Neg x -> preds x
  And x -> join (map preds x)
  Or x -> join (map preds x)
  Xor x y -> preds x ++ preds y
  Atom x -> [x]

---------------------------------------

data NameIndex = NameIndex {
  _predNames :: Map.Map (Text.Text,Int) PredName,
  _funNames :: Map.Map (Text.Text,Int) FunName
} deriving(Show)
makeLenses ''NameIndex

emptyNI = NameIndex Map.empty Map.empty

data State = State {
  _names :: NameIndex,
  _varStack :: [Text.Text]
}

makeLenses ''State

type M = StateM.StateT State (ExceptM.Except String)
type RM = ReaderM.ReaderT RevNameIndex (ExceptM.Except String)

-- ni.predNames[prefix?,arity] = ni.predNames.size()
allocName :: Num name => String -> Int -> Map.Map (Text.Text,Int) name -> (name, Map.Map (Text.Text,Int) name)
allocName prefix arity ni = (n, ni & at (Text.pack (prefix <> show x), arity) ?~ n) where
  n = fromIntegral x
  x = Map.size ni :: Int

lookupFunName :: (Text.Text,Int) -> M FunName
lookupFunName name = do
  mx <- use $ names.funNames.at name
  case mx of { Just x -> return x; _ -> do
    x <- use $ names.funNames.to (fromIntegral . Map.size);
    names.funNames.at name ?= x;
    return x;
  }

lookupTVar :: Text.Text -> M VarName
lookupTVar name = do
  mi <- use $ varStack.to (List.elemIndex name)
  case mi of
    Just i -> return (fromIntegral i)
    Nothing -> fail ("variable " ++ show name ++ " not bound")

runM :: M a -> NameIndex -> Either String (a,NameIndex)
runM ma ni = case (ExceptM.runExcept $ StateM.runStateT ma (State ni [])) of
  Left e -> Left e
  Right (a,s) -> Right (a,s^.names)

runRM :: RM a -> NameIndex -> Either String a
runRM ma ni = ExceptM.runExcept (ReaderM.runReaderT ma (revNI ni))

liftRM :: RM a -> M a
liftRM ma = use names >>= (lift . ReaderM.runReaderT ma . revNI)

------------------------------------------------------

fromProto :: T.File -> Either String Form
fromProto f = case runM (fromProto'File f) emptyNI of { Left e -> Left e; Right (f,ni) -> Right f }

fromProto'File :: T.File -> M Form
fromProto'File f = mapM fromProto'Input (f^. #input) >>= return.Or

fromProto'Input :: T.Input -> M Form
fromProto'Input i = do
  let { freeVars = unique $ freeVars'Formula (i^. #formula) };
  case i^. #language of {
    T.Input'CNF -> return ();
    T.Input'FOF -> when (freeVars/=mempty) $ fail "unexpected free vars in FOF";
    language@_ -> fail ("unexpected language: " ++ show language);
  };
  form <- push freeVars (fromProto'Form (i^. #formula)) >>= (\f -> return $ foldl (\x _-> Forall x) f freeVars);
  case i^. #role of {
    T.Input'AXIOM -> return $ Neg form;
    T.Input'PLAIN -> return $ Neg form;
    T.Input'NEGATED_CONJECTURE -> return $ Neg form;
    T.Input'CONJECTURE -> return form; 
    role@_ -> fail ("unexpected role: " ++ show role);
  };

fromProto'Form :: T.Formula -> M Form
fromProto'Form f =
  case f^. #maybe'formula of 
    Nothing -> fail "field missing"
    Just (T.Formula'Pred' pred) -> fromProto'Pred (pred) >>= return . Atom
    Just (T.Formula'Quant' quant) -> do {
      c <- (case (quant^. #type') of
        T.Formula'Quant'FORALL -> return Forall
        T.Formula'Quant'EXISTS -> return Exists
        _ -> fail "Formula'Quant'UNKNOWN");
      let { vars = quant^. #var };
      f <- push vars (fromProto'Form (quant^. #sub));
      return $ foldl (\x _-> c x) f vars
    }
    Just (T.Formula'Op op) -> do { 
      let { args2pair args = case args of { [l,r] -> return (l,r); _ -> fail "args != [l,r]" }};
      args <- mapM fromProto'Form (op^. #args);
      case (op^. #type') of
        T.Formula'Operator'NEG -> do {
          arg <- (case args of
            [h] -> return h
            _ -> fail "args != [h]");
          return (Neg arg)
        }
        T.Formula'Operator'OR -> return (Or args)
        T.Formula'Operator'AND -> return (And args)
        T.Formula'Operator'IFF -> do {
          (l,r) <- args2pair args;
          return (Neg (Xor l r));
        }
        T.Formula'Operator'IMPL -> do {
          (l,r) <- args2pair args;
          return (Or [(Neg l),r]);
        }
        T.Formula'Operator'XOR -> do {
          (l,r) <- args2pair args;
          return (Xor l r);
        }
        T.Formula'Operator'NOR -> return (Neg (Or args))
        T.Formula'Operator'NAND -> return (Neg (And args))
        T.Formula'Operator'TRUE -> return (And [])
        T.Formula'Operator'FALSE -> return (Or [])
        op@_ -> fail ("unexpected operator:" ++ show op)
    }

fromProto'Pred :: T.Formula'Pred -> M Pred
fromProto'Pred pred = do
  args <- mapM fromProto'Term (pred^. #args)
  case (pred^. #type') of
    T.Formula'Pred'CUSTOM -> do {
      name <- lookupPredName (pred^. #name, length args);
      return (wrap $ PCustom name args);
    }
    T.Formula'Pred'EQ -> case args of
      [l,r] -> return (wrap $ PEq l r)
      _ -> fail "args != [l,r]"
    _ -> fail "pred.type unknown"

fromProto'Term :: T.Term -> M Term
fromProto'Term term = case (term^. #type') of
  T.Term'VAR -> lookupTVar (term^. #name) >>= return.wrap.TVar
  T.Term'EXP -> do {
    args <- mapM fromProto'Term (term^. #args);
    name <- lookupFunName (term^. #name, length args);
    return (wrap $ TFun name args);
  }
  _ -> fail "term.type unknown"

toProto'Pred :: Ctx -> Pred -> Err T.Formula'Pred
toProto'Pred ctx pred = case unwrap pred of
  PEq l r -> do
    args' <- mapM (toProto'Term ctx) [l,r]
    return $ defMessage & #type' .~ T.Formula'Pred'EQ & #args .~ args'
  PCustom pn args -> do
    name <- orFail (printf "ctx.preds[%s] = Nothing" (show pn)) (ctx^.fromPredNames.at pn)
    args' <- mapM (toProto'Term ctx) args
    return $ defMessage & #type' .~ T.Formula'Pred'CUSTOM & #name .~ (Text.pack name) & #args .~ args'

toProto'Term :: Ctx -> Term -> Err T.Term
toProto'Term ctx term = case unwrap term of
  TVar vn -> return $ defMessage & #type' .~ T.Term'VAR & #name .~ Text.pack (show vn)
  TFun fn args -> do
    name <- orFail (printf "ctx.funs[%s] = Nothing" (show fn)) (ctx^.fromFunNames.at fn)
    args' <- mapM (toProto'Term ctx) args
    return $ defMessage & #type' .~ T.Term'EXP & #name .~ (Text.pack name) & #args .~ args'

freeVars'Term :: T.Term -> [Text.Text]
freeVars'Term t = case t^. #type' of {
  T.Term'VAR -> [t^. #name];
  T.Term'EXP -> t^. #args >>= freeVars'Term;
}

freeVars'Formula :: T.Formula -> [Text.Text]
freeVars'Formula f = case f^. #maybe'formula of {
  Nothing -> [];
  Just (T.Formula'Pred' pred) -> pred^. #args >>= freeVars'Term;
  Just (T.Formula'Quant' quant) -> Set.toAscList $ Set.fromList (freeVars'Formula $ quant^. #sub) \\ Set.fromList (quant^. #var);
  Just (T.Formula'Op op) -> op^. #args >>= freeVars'Formula;
}

