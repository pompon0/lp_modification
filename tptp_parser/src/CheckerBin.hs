{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
module CheckerBin(main) where

import Lib
import System.Environment(getArgs)
import qualified Data.ProtoLens.TextFormat as TextFormat
import Data.ProtoLens.Message(Message)
import qualified Data.Text.Lazy as Text
import qualified Proto.Tptp as T
import qualified Proto.Solutions as SPB
import qualified Proof
import qualified Form
import qualified NNF
import qualified DNF
import qualified DefDNF
import Skolem
import Valid(counterExample)
import qualified Parser
import qualified Trace

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Lens

formToDNF :: (Ctx,Form.Form) -> (Ctx,DNF.OrForm)
formToDNF = DNF.simplify . DNF.dnf . Skolem.skol . NNF.nnf

--TODO: move the quantifiers down, convert to CNF (treating quantified formulas as atoms),
--  which will give you a decomposition into subproblems
toDNF :: T.File -> Either String DNF.OrForm
toDNF tptpFile = fmap formToDNF (Form.fromProto tptpFile) 

readAndParse :: String -> IO T.File
readAndParse tptp_path = do
  content <- readFile tptp_path
  Trace.evalIO (Parser.parse content) >>= assert

help = do
  putStrLn "conv [fof|cnf] [tptp_file] > [proto_file]"
  putStrLn "cnf [reg|def] [fof_proto_file] > [cnf_proto_file]"
  putStrLn "validate [solution_proto_file] > [stats_proto_file]"


conv [language,tptp_path] = do
  case language of {
    "fof" -> readAndParse tptp_path >>= putStrLn . TextFormat.showMessage;
    "cnf" -> do {
      file <- readAndParse tptp_path;
      (dnf,ni) <- assert $ DNF.fromProto file Form.emptyNI;
      assert (DNF.toProto dnf ni) >>= putStrLn . TextFormat.showMessage;
    };
    _ -> help;
  }

cnf [mode,fof_proto_file] = do
  file <- readProtoFile fof_proto_file
  (fof,ni) <- assert $ Form.runM (Form.fromProto'File file) Form.emptyNI
  case mode of
    "reg" -> assert (DNF.toProto (formToDNF fof) ni) >>= putStrLn . TextFormat.showMessage
    "def" -> assert (DNF.toProto dnf ni') >>= putStrLn . TextFormat.showMessage where
      (dnf,ni') = DefDNF.defDNF (NNF.nnf fof) ni

validate [solution_proto_file] = do
  solutionProto :: SPB.CNF <- readProtoFile solution_proto_file
  ((problem,proof,stats),_) <- assert $ Form.runM (do
    problem <- DNF.fromProto'File (solutionProto^. #problem)
    proof <- DNF.fromProto'File (solutionProto^. #proof)
    stats <- Form.liftRM $ Proof.classify proof problem
    case counterExample proof of
      Nothing -> return ()
      Just x -> fail ("counter example: " ++ show x)
    return (problem,proof,stats)) Form.emptyNI
  putStrLnE ("problem = " ++ show problem)
  putStrLnE ("proof = " ++ show proof)
  putStrLn (TextFormat.showMessage stats)

main = do
  cmd:args <- getArgs
  case cmd of {
    "conv" -> conv args;
    "cnf" -> cnf args;
    "validate" -> validate args;
    _ -> help;
  }
