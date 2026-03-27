module ParserTests (full_test) where

import Control.Monad (forM_)
import Data.Either (isLeft)
import Data.List (intercalate, isInfixOf, isPrefixOf)
import GHC.IO.Exception (ExitCode)
import Gen.BoaProgram
import Interpreter
import Parser
import Syntax
import System.IO
import System.Process
import qualified Test.QuickCheck.Monadic as Monadic
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.Parsec (parse)
import Text.ParserCombinators.Parsec (parseTest)

runPythonCode :: String -> IO (String, String, ExitCode)
runPythonCode code = do
    (Just hin, Just hout, Just herr, ph) <-
        createProcess
            (proc "python" [])
                { std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = CreatePipe
                }
    hPutStr hin code
    hClose hin

    out <- hGetContents hout
    err <- hGetContents herr

    exitCode <- waitForProcess ph
    return (out, err, exitCode)

firstDiff :: String -> String -> Maybe (Int, Char, Char)
firstDiff l r = findDiff l r 0
  where
    findDiff :: String -> String -> Int -> Maybe (Int, Char, Char)
    findDiff [] [] _ = Nothing
    findDiff [] (x : _) n = Just (n, x, '\0')
    findDiff (x : _) [] n = Just (n, x, '\0')
    findDiff (x : xs) (y : ys) n = if x == y then findDiff xs ys (n + 1) else Just (n, x, y)

replaceAll :: String -> String -> String -> String
replaceAll old new = replace'
  where
    replace' [] = []
    replace' s@(x : xs)
        | old `isPrefixOf` s = new ++ replace' (drop (length old) s)
        | otherwise = x : replace' xs

diffAt :: String -> String -> Int -> (String, String, Int)
diffAt [] r n = ("<End>", r, n)
diffAt l [] n = (l, "<End>", n)
diffAt (l : ls) (r : rs) n
    | l == r = diffAt ls rs (n + 1)
    | otherwise = (l : ls, r : rs, n)

test :: BoaProgram -> Property
test p = Monadic.monadicIO $ do
    let (P code) = p
        pythonsource =
            "def listrange(*x):\n  return list(range(*x))\n"
                ++ replaceAll "range" "listrange" code

    (stdout_py, stderr_py, _) <- Monadic.run (runPythonCode pythonsource)

    boaOutput <- case parseString code of
        Left e -> return $ Left e
        Right p -> return $ Right $ execute p

    let (caseTag, stdout_boa, stderr_boa, ok) = case boaOutput of
            Left e -> ("parse-failed", "", show e, False)
            Right (o, e) ->
                let stdout_boa = if null o then "" else intercalate "\n" o ++ "\n"
                 in case e of
                        Nothing ->
                            ("no-error", stdout_boa, "", stdout_py == stdout_boa)
                        Just e ->
                            if stdout_py == stdout_boa
                                then
                                    ( "runtime-error-same-stdout"
                                    , stdout_boa
                                    , show e
                                    , ("Traceback" `isInfixOf` stderr_py)
                                    )
                                else ("runtime-error-different-stdout", stdout_boa, show e, False)
        (boa_diff, python_diff, diffIndex) = diffAt stdout_boa stdout_py 0

    Monadic.monitor $ label caseTag

    Monadic.monitor $ cover 20 (caseTag /= "parse-failed") "Parsed successfully"
    Monadic.monitor $ cover 1 (caseTag == "no-error") "No errors"

    Monadic.monitor $
        counterexample $
            "\n\ncase: "
                ++ caseTag
                ++ "\nstdout_py:\n"
                ++ stdout_py
                ++ "\nstderr_py:\n"
                ++ stderr_py
                ++ "\nstdout_boa:\n"
                ++ stdout_boa
                ++ "\nstderr_boa:\n"
                ++ stderr_boa
                ++ if caseTag /= "parse-failed"
                    then
                        "\n\n\nFirst different character at "
                            ++ show diffIndex
                            ++ "\nPython output first char diff:\n"
                            ++ take 20 python_diff
                            ++ " [...]"
                            ++ "\nBoa output first char diff:\n"
                            ++ take 20 boa_diff
                            ++ " [...]"
                    else ""

    Monadic.assert ok

assertLeft :: Either a b -> Assertion
assertLeft = assertBool "expected left" . isLeft

checkSuccess :: Program -> Either ParseError Program -> Bool
checkSuccess _ (Left _) = False
checkSuccess p1 (Right p2) = p1 == p2

testMany :: [String] -> [Either ParseError Program -> Bool] -> String -> IO ()
testMany sources tests assertionMessage = forM_ (zip sources tests) $ \(src, test) ->
    assertBool (assertionMessage ++ src) $ test $ parseString src

full_test :: TestTree
full_test =
    testGroup
        "Boa test suite"
        [ testCase
            "Assignment"
            $ parseString "a1_4 = 5" @?= Right [Define "a1_4" $ Constant $ Number 5]
        , testCase
            "Identifier not starting with number"
            $ assertLeft
            $ parseString "1a = 5"
        , testCase
            "Reserved keywords"
            $ testMany
                ["None = 1", "True = 1", "False = 1", "for = 1", "if = 1", "in = 1", "not = 1"]
                (repeat isLeft)
                "Should fail to parse assignment to reserved keywords: "
        , testCase "Well-formed numbers" $
            testMany
                ["123", "-78", "-0", "100"]
                [ checkSuccess [Execute $ Constant $ Number 123]
                , checkSuccess [Execute $ Constant $ Number $ -78]
                , checkSuccess [Execute $ Constant $ Number 0]
                , checkSuccess [Execute $ Constant $ Number 100]
                ]
                "Should parse to constant: "
        , testCase "Badly formed numbers" $
            testMany
                ["007", "+2\"", "- 4"]
                (repeat isLeft)
                "Should fail to parse badly formed numbers: "
        , testCase "badlyFormedString" $
            testMany
                [ "\"Hello\""
                , "'\n'"
                , "'\\'"
                , "'\\r'"
                , "'\r'"
                ]
                (repeat isLeft)
                "Should fail to parse badly formed string: "
        , testProperty "Same result as python" test
        ]
