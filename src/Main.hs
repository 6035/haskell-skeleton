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
import System.IO (hPutStr, hPutStrLn, stderr)

main :: IO ()
main = do
  eitherConf <- getConfiguration
  case eitherConf of
    Left err -> do
      hPutStrLn stderr err
      generateUsage >>= hPutStr stderr
    Right conf -> print conf
