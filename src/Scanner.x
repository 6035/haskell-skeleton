-- Scanner -- Decaf scanner                                     -*- haskell -*-
-- Copyright (C) 2013  Benjamin Barenblat <bbaren@mit.edu>
--
-- This file is a part of decafc.
--
-- decafc is free software: you can redistribute it and/or modify it under the
-- terms of the MIT (X11) License as described in the LICENSE file.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the X11 license for more details.
{
{-# OPTIONS_GHC -w #-}
module Scanner ( ScannedToken, line, token
               , Token(..)
               , scanTokens
               ) where
}

%wrapper "posn"                 -- keep track of line numbers


----------------------------------- Tokens ------------------------------------

$alpha = [a-zA-Z]

tokens :-
  $white+ ;
  "//".*  ;                     -- comment
  class   { \posn s -> scannedToken posn $ Keyword s }
  \{      { \posn _ -> scannedToken posn LCurly }
  \}      { \posn _ -> scannedToken posn RCurly }
  $alpha+ { \posn s -> scannedToken posn $ Identifier s }


----------------------------- Representing tokens -----------------------------

{
-- | A token with position information.
data ScannedToken = ScannedToken { line :: Int
                                 , token :: Token
                                 } deriving (Eq, Show)

-- | A token.
data Token = Keyword String
           | Identifier String
           | LCurly
           | RCurly
           deriving (Eq)
instance Show Token where
  show (Keyword k) = "KEYWORD " ++ k
  show (Identifier s) = "IDENTIFIER " ++ s
  show LCurly = "{"
  show RCurly = "}"

{-| Smart constructor to create a 'ScannedToken' by extracting the line number
from an 'AlexPosn'. -}
scannedToken :: AlexPosn -> Token -> ScannedToken
scannedToken (AlexPn _ lineNo _) tok = ScannedToken lineNo tok


---------------------------- Scanning entry point -----------------------------

scanTokens :: String -> [ScannedToken]
scanTokens = alexScanTokens
}
