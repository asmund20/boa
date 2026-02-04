-- Skeleton file for Boa Parser.

module Parser (ParseError, parseString) where

import Syntax
-- add any other other imports you need

type ParseError = String -- you may replace this

parseString :: String -> Either ParseError Program
parseString = undefined  -- define this

