module ParserTests (full_test) where

import Data.List (intercalate, isInfixOf, isPrefixOf)
import GHC.IO.Exception (ExitCode)
import Gen.BoaProgram
import Interpreter
import Parser
import System.IO
import System.Process
import qualified Test.QuickCheck.Monadic as Monadic
import Test.Tasty
import Test.Tasty.QuickCheck
import Text.Parsec (parse)

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
diffAt [] r n = ("", r, n)
diffAt l [] n = (l, "", n)
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
            Left e -> ("parse-failed", "", show e, False) -- "SyntaxError" `isInfixOf` stderr_py)
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
                ++ if caseTag /= "parse-failed" && not (null boa_diff) && not (null python_diff)
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

full_test :: TestTree
full_test =
    testGroup
        "Boa test suite"
        [ testProperty "Same result as python" test
        ]
