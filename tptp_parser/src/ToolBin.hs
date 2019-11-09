{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
module Main where

import System.Environment(getArgs)
import qualified Data.ProtoLens.TextFormat as TextFormat
import Data.ProtoLens.Message(Message)
import qualified Proto.Tptp as T
import qualified Proto.Solutions as SPB
import qualified Proof

import Ctx
import qualified FOF
import NNF
import DNF
import DefDNF
import Valid(counterExample)
import qualified Parser
import IO

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Lens
import qualified Data.Text.Lazy as Text

readProtoFile :: Message a => String -> IO a
readProtoFile path = readFile path >>= assert . Err . TextFormat.readMessage . Text.pack 

--------------------------------

fof'dnf :: (Global,FOF.FOF) -> (GlobalVar,OrForm)
fof'dnf (g,fof) = (gv,simplify dnf) where
  (gv,dnf) = nnf'dnf (g, fof'nnf fof)

fof'dnf'def :: (Global,FOF.FOF) -> (GlobalVar,OrForm)
fof'dnf'def (g,fof) = (gv,simplify dnf) where
  (gv,dnf) = nnf'dnf'def (g, fof'nnf fof)

assert :: Err a -> IO a
assert (Err ea) = case ea of { Left e -> fail e; Right a -> return a }

readAndParse :: String -> IO T.File
readAndParse tptp_path = do
  content <- readFile tptp_path
  assert (Parser.parse content)

help = do
  putStrLn "conv [fof|cnf] [tptp_file] > [proto_file]"
  putStrLn "cnf [reg|def] [fof_proto_file] > [cnf_proto_file]"
  putStrLn "validate [solution_proto_file] > [stats_proto_file]"


conv [language,tptp_path] = do
  case language of {
    "fof" -> readAndParse tptp_path >>= putStrLn . TextFormat.showMessage;
    "cnf" -> do {
      file <- readAndParse tptp_path;
      let { gv = FOF.make'GlobalVar [file] };
      dnf <- assert $ fromProto'File gv file;
      putStrLn $ TextFormat.showMessage $ toProto'File dnf;
    };
    _ -> help;
  }

cnf [mode,fof_proto_file] = do
  putStrLnE "doing cnf..."
  file <- readProtoFile fof_proto_file
  let g = FOF.make'Global [file]
  fof <- assert $ FOF.fromProto'File g file
  let f = case mode of { "reg" -> fof'dnf; "def" -> fof'dnf'def }
  putStrLnE "starting conversion..."
  let (gv,dnf) = f (g,fof)
  putStrLnE "conversion done..."
  putStrLn $ TextFormat.showMessage $ toProto'File dnf
  putStrLnE "dnf returned"

validate [solution_proto_file] = do
  solutionProto :: SPB.CNF <- readProtoFile solution_proto_file
  (problem,proof,stats) <- assert $ do
    let gv = FOF.make'GlobalVar [solutionProto^. #problem, solutionProto^. #proof]
    problem <- fromProto'File gv (solutionProto^. #problem)
    proof <- fromProto'File gv (solutionProto^. #proof)
    stats <- Proof.classify proof problem
    case counterExample proof of
      Nothing -> return ()
      Just x -> fail ("counter example: " ++ show x)
    return (problem,proof,stats)
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
