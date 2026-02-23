module Interpreter where

-- Skeleton file for Boa Interpreter. Edit only definitions with 'undefined'

import Control.Monad
import Data.List (find)
import Syntax

type Output = [String]

type ErrorMessage = String
data RuntimeError
    = UnboundVariable VariableName
    | BadFunction FunctionName
    | BadArgument ErrorMessage
    deriving (Eq, Show)

type Environment = [(VariableName, Value)]
type Runtime a = Environment -> (Either RuntimeError a, Output)

newtype Boa a = Boa {run :: Runtime a}

-- Helper function for bind definition
unwrap :: Boa a -> Runtime a
unwrap (Boa b) = b

-- Monad instance.
instance Monad Boa where
    return = pure -- defined in Applicative instance because the lsp wants it
    -- (>>=) :: Boa a -> (a -> Boa b) -> Boa b

    b1 >>= b2 =
        Boa
            ( \env ->
                let (x1, o1) = run b1 env
                    (x2, o2) = case x1 of
                        Left l -> (Left l, [])
                        Right r -> run (b2 r) env
                 in (x2, o1 <> o2)
            )
instance Functor Boa where
    fmap = liftM
instance Applicative Boa where
    pure x = Boa (const (Right x, []))
    (<*>) = ap

-- Operations of the Boa monad
abort :: RuntimeError -> Boa a
abort re = Boa (const (Left re, []))

look :: VariableName -> Boa Value
look var =
    Boa
        ( \env ->
            ( case find (\(f, _) -> f == var) env of
                Nothing -> Left $ UnboundVariable var
                Just (_, v) -> Right v
            , []
            )
        )

bind :: VariableName -> Value -> (Boa a -> Boa a)
bind = undefined

output :: String -> Boa ()
output s = Boa (const (Right mempty, [s]))

-- Helper functions for interpreter
truthy :: Value -> Bool
truthy None = False
truthy (Boolean b) = b
truthy (Number 0) = False
truthy (Text []) = False
truthy (List []) = False
truthy _ = True

boolToInt :: Bool -> Integer
boolToInt False = 0
boolToInt True = 1

operate :: OperationSymbol -> Value -> Value -> Either ErrorMessage Value
operate op None _ = Left $ "Operator " ++ show op ++ " with None in left argument."
operate op _ None = Left $ "Operator " ++ show op ++ " with None in right argument."
operate Plus (Number l) (Number r) = Right $ Number $ l + r
operate Plus (Boolean l) (Boolean r) = Right $ Number $ (boolToInt l) + (boolToInt r)
operate Plus (Boolean l) (Number r) = Right $ Number $ (boolToInt l) + r
operate Plus (Number l) (Boolean r) = Right $ Number $ l + (boolToInt r)
operate Plus (List l) (List r) = Right $ List $ l ++ r
operate Plus (Text l) (Text r) = Right $ Text $ l ++ r
operate Minus (Number l) (Number r) = Right $ Number $ l - r
operate Minus (Boolean l) (Boolean r) = Right $ Number $ (boolToInt l) - (boolToInt r)
operate Minus (Boolean l) (Number r) = Right $ Number $ (boolToInt l) - r
operate Minus (Number l) (Boolean r) = Right $ Number $ l - (boolToInt r)
operate Times (Number l) (Number r) = Right $ Number $ l * r
operate Times (Boolean l) (Boolean r) = Right $ Number $ (boolToInt l) * (boolToInt r)
operate Times (Boolean l) (Number r) = Right $ Number $ (boolToInt l) * r
operate Times (Number l) (Boolean r) = Right $ Number $ l * (boolToInt r)
operate Div (Number l) (Number r) = Right $ Number $ l `div` r
operate Div (Boolean l) (Boolean r) = Right $ Number $ (boolToInt l) `div` (boolToInt r)
operate Div (Boolean l) (Number r) = Right $ Number $ (boolToInt l) `div` r
operate Div (Number l) (Boolean r) = Right $ Number $ l `div` (boolToInt r)
operate Mod (Number l) (Number r) = Right $ Number $ l `mod` r
operate Mod (Boolean l) (Boolean r) = Right $ Number $ (boolToInt l) `mod` (boolToInt r)
operate Mod (Boolean l) (Number r) = Right $ Number $ (boolToInt l) `mod` r
operate Mod (Number l) (Boolean r) = Right $ Number $ l `mod` (boolToInt r)
operate Eq (Number l) (Number r) = Right $ Boolean $ l == r
operate Eq (Boolean l) (Number r) = Right $ Boolean $ (boolToInt l) == r
operate Eq (Number l) (Boolean r) = Right $ Boolean $ l == (boolToInt r)
operate Eq (Boolean l) (Boolean r) = Right $ Boolean $ l == r
operate Eq (Text l) (Text r) = Right $ Boolean $ l == r
operate Eq (List l) (List r) = Right $ Boolean $ l == r
operate Less (Boolean l) (Boolean r) = Right $ Boolean $ l < r
operate Less (Number l) (Number r) = Right $ Boolean $ l < r
operate Less (Text l) (Text r) = Right $ Boolean $ l < r
operate Less (List l) (List r) = Right $ Boolean $ length l < length r
operate Greater (Boolean l) (Boolean r) = Right $ Boolean $ l > r
operate Greater (Number l) (Number r) = Right $ Boolean $ l > r
operate Greater (Text l) (Text r) = Right $ Boolean $ l > r
operate Greater (List l) (List r) = Right $ Boolean $ length l > length r
operate In (List l) (List ((List r) : rs)) = Right $ Boolean $ elem (List l) ((List r) : rs)
operate In (Number l) (List ((Number r) : rs)) = Right $ Boolean $ elem (Number l) ((Number r) : rs)
operate In (Text l) (List ((Text r) : rs)) = Right $ Boolean $ elem (Text l) ((Text r) : rs)
operate In (Boolean l) (List ((Boolean r) : rs)) = Right $ Boolean $ elem (Boolean l) ((Boolean r) : rs)
operate op v1 v2 = Left $ "Operator " ++ show op ++ " with arguments " ++ show v1 ++ ", " ++ show v2 ++ "."

apply :: FunctionName -> FunctionArguments -> Boa Value
apply = undefined

-- Main functions of interpreter
eval :: Expression -> Boa Value
eval (Operation op e1 e2) =
    do
        v1 <- eval e1
        v2 <- eval e2
        case operate op v1 v2 of
            (Right value) -> return value
            (Left err) -> abort $ BadArgument err
eval _ = undefined

exec :: Program -> Boa ()
exec [] = return ()
exec (Define x body : program) =
    do
        v <- eval body
        bind x v (exec program)
exec (Execute thing : program) =
    do
        eval thing
        exec program

execute :: Program -> (Output, Maybe RuntimeError)
execute = undefined
