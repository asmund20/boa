-- Skeleton file for Boa Parser.

module Parser (ParseError, parseString) where

import Syntax
import Text.ParserCombinators.Parsec

-- add any other other imports you need

parseString :: String -> Either ParseError Program
parseString = undefined -- define this
