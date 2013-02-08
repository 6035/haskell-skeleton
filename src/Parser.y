-- Parser -- Decaf parser                                       -*- haskell -*-
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
module Parser ( parse
              ) where

import Scanner (Token(..))

}


--------------------------------- Directives ----------------------------------

%name parse
%error { parseError }
%monad { Either String }

%tokentype { Token }

%token
  class      { Keyword "class" }
  identifier { Identifier $$ }
  '{'        { LCurly }
  '}'        { RCurly }


%% -------------------------------- Grammar -----------------------------------

Program : class identifier '{' '}' { Program $2 }


----------------------------------- Haskell -----------------------------------
{
data Program = Program { className :: String
                       } deriving (Eq, Show)

parseError :: [Token] -> Either String a
parseError toks = Left $ show toks
}
