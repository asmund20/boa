module InterpreterTests where

import Interpreter
import Syntax
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

interpreter_tests :: TestTree
interpreter_tests =
    testGroup
        "Example Test"
        [ testCase "crash test" $
            execute
                [ Execute
                    ( Call
                        "print"
                        [ Operation
                            Plus
                            (Constant (Number 2))
                            (Constant (Number 2))
                        ]
                    )
                , Execute (Variable "hello")
                ]
                @?= (["4"], Just (UnboundVariable "hello"))
        , testCase "execute misc.boa.ast example folder" $
            do
                pgm <- read <$> readFile "examples/misc.boa.ast"
                out <- readFile "examples/misc.boa.output"
                execute pgm @?= (lines out, Nothing)
        , testCase "execute primes.boa.ast example folder" $
            do
                pgm <- read <$> readFile "examples/primes.boa.ast"
                out <- readFile "examples/primes.boa.output"
                execute pgm @?= (lines out, Nothing)
        , testCase "execute hello_world.boa.ast example folder" $
            do
                pgm <- read <$> readFile "examples/hello_world.boa.ast"
                out <- readFile "examples/hello_world.boa.output"
                execute pgm @?= (lines out, Nothing)
        , testCase "execute or.boa.ast example folder" $
            do
                pgm <- read <$> readFile "examples/or.boa.ast"
                out <- readFile "examples/or.boa.output"
                execute pgm @?= (lines out, Nothing)
        ]
