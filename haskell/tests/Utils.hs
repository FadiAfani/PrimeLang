module Utils where

import Token
import ScopeStack

mkDummyToken :: String -> TokenType -> Token
mkDummyToken = Token (0,0)

mkDummySymbol :: Symbol
mkDummySymbol = Symbol [] Nothing 0 False
