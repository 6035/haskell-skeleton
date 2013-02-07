{- Scanner.Pretty -- pretty-print tokens
Copyright (C) 2013  Benjamin Barenblat <bbaren@mit.edu>

This file is a part of decafc.

decafc is free software: you can redistribute it and/or modify it under the
terms of the MIT (X11) License as described in the LICENSE file.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the X11 license for more details. -}
module Scanner.Pretty ( format, formatOne
                      , print, printOne
                      ) where

import Prelude hiding (print)

import Scanner


------------------------------ Formatting tokens ------------------------------

format :: [ScannedToken] -> [String]
format = map formatOne

formatOne :: ScannedToken -> String
formatOne tok = unwords [ show $ line tok
                        , show $ token tok ]


------------------------------- Printing tokens -------------------------------

print :: [ScannedToken] -> IO ()
print = mapM_ printOne

printOne :: ScannedToken -> IO ()
printOne = putStrLn . formatOne
