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

import Err
import Ctx
import qualified FOF
import DNF
import Valid(counterExample)
import qualified Parser
import IO
import Tptp
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Lens
import qualified Data.Text.Lazy as Text

readProtoFile :: Message a => String -> IO a
readProtoFile path = assert . Err . decodeMessage =<< readFile path

--------------------------------

help = do
  putStrLn "tptp [proto_file] > [tptp_file]"
  putStrLn "validate [solution_proto_file] > [stats_proto_file]"

tptp [proto_path] = do
  file :: T.File <- readProtoFile proto_path
  putStrLn =<< assert (Parser.prettyPrint file ??? "prettyPrint")

flattenProof :: SPB.Proof -> T.File
flattenProof p = defMessage
  & #input .~ (p^.. #clauses.traverse. #sources.traverse. #ground)
  & #nodes .~ (p^. #nodes)

validate [solution_proto_file] = do
  solutionProto :: SPB.CNF <- readProtoFile solution_proto_file
  (problem,proof,stats) <- assert $ do
    flatProofProto <-r$ flattenProof (solutionProto^. #proof)
    -- # build a common index to make sure that node sets are consistent
    idx <- nodes'index (solutionProto^. #problem. #nodes) ??? "nodes'index problem"
    idx <- index'merge idx =<< nodes'index (flatProofProto^. #nodes) ??? "nodes'index proof"
    problem <- DNF.fromProto'File (solutionProto^. #problem)
    proof <- DNF.fromProto'File flatProofProto
    stats <- Proof.classify idx proof problem
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
