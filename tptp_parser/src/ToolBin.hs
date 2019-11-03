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

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Lens
import Control.Monad.IO.Class(MonadIO,liftIO)
import qualified Data.Text.Lazy as Text
import System.IO(hFlush,hPutStrLn,stdout,stderr)

readProtoFile :: Message a => String -> IO a
readProtoFile path = readFile path >>= assert . Err . TextFormat.readMessage . Text.pack 

putStrLnE :: MonadIO m => String -> m ()
putStrLnE s = liftIO (hPutStrLn stderr s >> hFlush stderr)
printE :: (MonadIO m, Show a) => a -> m ()
printE x = putStrLnE (show x)

--------------------------------

fof'dnf :: (Global,FOF.FOF) -> (GlobalVar,OrForm)
fof'dnf (g,fof) = (gv,simplify dnf) where
  (gv,dnf) = nnf'dnf (g, fof'nnf fof)

fof'dnf'def :: (Global,FOF.FOF) -> (GlobalVar,OrForm)
fof'dnf'def (g,fof) = (gv,simplify dnf) where
  (gv,dnf) = nnf'dnf'def (g, fof'nnf fof)

--TODO: move the quantifiers down, convert to CNF (treating quantified formulas as atoms),
--  which will give you a decomposition into subproblems
file'dnf :: T.File -> Err (GlobalVar,OrForm)
file'dnf tptpFile = fof'dnf <$> FOF.fromProto'File tptpFile

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
      (gv,dnf) <- assert $ fromProto'File file;
      putStrLn $ TextFormat.showMessage $ toProto'File dnf;
    };
    _ -> help;
  }

cnf [mode,fof_proto_file] = do
  file <- readProtoFile fof_proto_file
  (g,fof) <- assert $ FOF.fromProto'File file
  let f = case mode of { "reg" -> fof'dnf; "def" -> fof'dnf'def }
  let (gv,dnf) = f (g,fof)
  putStrLn $ TextFormat.showMessage $ toProto'File dnf

validate [solution_proto_file] = do
  solutionProto :: SPB.CNF <- readProtoFile solution_proto_file
  (problem,proof,stats) <- assert $ do
    (g,problem) <- fromProto'File (solutionProto^. #problem)
    (_,proof) <- fromProto'File (solutionProto^. #proof)
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
