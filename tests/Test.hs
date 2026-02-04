-- Skeleton test suite using Tasty.
-- Fell free to modify or replace anything in this file

import InterpreterTests
import Test.Tasty

main :: IO ()
main =
  defaultMain $
  localOption (mkTimeout 1000000) $
  testGroup "Test Suite :" $
  [ interpreter_tests
  ]




