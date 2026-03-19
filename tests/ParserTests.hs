module ParserTests (full_test) where

import Data.List (intercalate, isInfixOf)
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

test :: BoaProgram -> Property
test p = Monadic.monadicIO $ do
    let (P code) = p
    (stdout_py, stderr_py, _) <- Monadic.run (runPythonCode code)
    boaOutput <- case parseString code of
        Left e -> return $ Left e
        Right p -> return $ Right $ execute p

    case boaOutput of
        Left _ -> return $ "SyntaxError" `isInfixOf` stderr_py
        Right (o, e) -> case e of
            Nothing -> return $ stdout_py == (intercalate "\n" o)
            Just e -> return $ stdout_py == (intercalate "\n" o) && (not $ null stderr_py)

    return True

full_test :: TestTree
full_test =
    testGroup
        "Boa test suite"
        [ testProperty "Same result as python" test
        ]
