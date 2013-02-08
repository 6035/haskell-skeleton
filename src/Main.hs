{- Main -- main entry point
Copyright (C) 2013  Benjamin Barenblat <bbaren@mit.edu>

This file is a part of decafc.

decafc is free software: you can redistribute it and/or modify it under the
terms of the MIT (X11) License as described in the LICENSE file.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the X11 license for more details. -}
module Main where

import CLI (generateUsage, getConfiguration)
import Data.Either (partitionEithers)
import qualified System.Exit
import System.IO (hPutStr, hPutStrLn, stderr)

import Configuration (CompilerStage(..))
import qualified Configuration
import qualified Parser
import qualified Scanner
import qualified Scanner.Pretty

main :: IO ()
main = do
  eitherConf <- getConfiguration
  case eitherConf of
    Left err -> do
      -- Error on the command line.
      hPutStrLn stderr err
      generateUsage >>= fatal
    Right conf ->
      -- Okay, we actually should try to compile something.
      case Configuration.target conf of
        Scan -> do
          source <- readFile $ Configuration.input conf
          printTokens $ Scanner.scanTokens source
        Parse -> do
          source <- readFile $ Configuration.input conf
          case partitionEithers $ Scanner.scanTokens source of
            ([], scannedTokens) ->
              print $ Parser.parseProgram $ map Scanner.token scannedTokens
            (errors, _) -> fatal $ concatMap (++"\n") errors
        _ -> error "not yet implemented"

fatal :: String -> IO ()
fatal message = do
  hPutStr stderr message
  System.Exit.exitFailure

printTokens :: [Either String Scanner.ScannedToken] -> IO ()
printTokens =
  mapM_ $ either (hPutStrLn stderr) (putStrLn . Scanner.Pretty.formatOne)
