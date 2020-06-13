{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
module Main where

import Prelude hiding(readFile)
import System.IO(stdout)
import System.Environment(getArgs)
import qualified Data.ProtoLens.TextFormat as TextFormat
import Data.ProtoLens(defMessage)
import Data.ProtoLens.Message(Message)
import Data.ProtoLens.Encoding(encodeMessage,decodeMessage)
import Data.ByteString(hPut,ByteString,readFile)
import qualified Data.ByteString.Char8 as Char8
import qualified Proto.Tptp as T
import qualified Proto.Solutions as SPB
import qualified Proof

import Ctx
import qualified FOF
import NNF
import DNF
import Valid(counterExample)
import qualified Parser
import IO
import qualified Tptp

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Lens
import qualified Data.Text.Lazy as Text

assert :: Err a -> IO a
assert (Err ea) = case ea of { Left e -> fail e; Right a -> return a }

readProtoFile :: Message a => String -> IO a
readProtoFile path = assert . Err . decodeMessage =<< readFile path

--------------------------------

help = do
  putStrLn "tptp [proto_file] > [tptp_file]"
  putStrLn "validate [solution_proto_file] > [stats_proto_file]"

tptp [proto_path] = do
  file <- readProtoFile proto_path
  putStrLn $ Parser.prettyPrint file

flattenProof :: SPB.Proof -> T.File
flattenProof p = defMessage & #input .~ (p^.. #clauses.traverse. #sources.traverse. #ground)

validate [solution_proto_file] = do
  solutionProto :: SPB.CNF <- readProtoFile solution_proto_file
  (problem,proof,stats) <- assert $ do
    let flatProofProto = flattenProof (solutionProto^. #proof)
    let gv = FOF.globalVar'make [solutionProto^. #problem, flatProofProto]
    problem <- DNF.fromProto'File gv (solutionProto^. #problem)
    proof <- DNF.fromProto'File gv flatProofProto
    stats <- Proof.classify proof problem
    case counterExample proof of
      Nothing -> return ()
      Just x -> fail ("counter example: " ++ show x)
    return (problem,proof,stats)
  putStrLnE ("problem = " ++ show problem)
  putStrLnE ("proof = " ++ show proof)
  hPut stdout $ encodeMessage stats

main = do
  cmd:args <- getArgs
  case cmd of {
    "tptp" -> tptp args;
    "validate" -> validate args;
    _ -> help;
  }
