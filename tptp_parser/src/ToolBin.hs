{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
module Main where

import Prelude hiding(readFile)
import System.IO(stdout)
import System.Environment(getArgs)
import qualified Data.ProtoLens.TextFormat as TextFormat
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
import DefDNF
import Valid(counterExample)
import qualified Parser2
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

fof'dnf :: (Global,FOF.FOF) -> (GlobalVar,OrForm)
fof'dnf (g,fof) = (gv,simplify dnf) where
  (gv,dnf) = nnf'dnf (g, fof'nnf fof)

fof'dnf'def :: (Global,FOF.FOF) -> (GlobalVar,OrForm)
fof'dnf'def (g,fof) = (gv,simplify dnf) where
  (gv,dnf) = nnf'dnf'def (g, fof'nnf fof)

readAndParse :: String -> IO T.File
readAndParse tptp_path = do
  content <- readFile tptp_path
  assert (Parser2.parse $ Char8.unpack content)

help = do
  putStrLn "tptp [proto_file] > [tptp_file]"
  putStrLn "conv [fof|cnf] [tptp_file] > [proto_file]"
  putStrLn "cnf [reg|def] [fof_proto_file] > [cnf_proto_file]"
  putStrLn "validate [solution_proto_file] > [stats_proto_file]"

tptp [proto_path] = do
  file <- readProtoFile proto_path
  putStrLn $ Parser2.prettyPrint file
  --putStrLn $ Tptp.file'string file

conv [language,tptp_path] = do
  case language of {
    "fof" -> readAndParse tptp_path >>= hPut stdout . encodeMessage;
    "cnf" -> do {
      file <- readAndParse tptp_path;
      let { gv = FOF.globalVar'make [file] };
      dnf <- assert $ fromProto'File gv file;
      hPut stdout $ encodeMessage $ toProto'File dnf;
    };
    _ -> help;
  }

cnf [mode,fof_proto_file] = do
  file <- readProtoFile fof_proto_file
  let g = FOF.global'make [file]
  fof <- assert $ FOF.fromProto'File g file
  let f = case mode of { "reg" -> fof'dnf; "def" -> fof'dnf'def }
  let (gv,dnf) = f (g,fof)
  hPut stdout $ encodeMessage $ toProto'File dnf

validate [solution_proto_file] = do
  solutionProto :: SPB.CNF <- readProtoFile solution_proto_file
  (problem,proof,stats) <- assert $ do
    let gv = FOF.globalVar'make [solutionProto^. #problem, solutionProto^. #proof]
    problem <- fromProto'File gv (solutionProto^. #problem)
    proof <- fromProto'File gv (solutionProto^. #proof)
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
    "conv" -> conv args;
    "cnf" -> cnf args;
    "validate" -> validate args;
    _ -> help;
  }
