-- Skeleton test suite using Tasty.
-- Fell free to modify or replace anything in this file

import InterpreterTests
import ParserTests
import Test.Tasty

main :: IO ()
main =
    defaultMain $
        localOption (mkTimeout 15000000) $
            testGroup "Test Suite :" $
                [ interpreter_tests
                , full_test
                ]
