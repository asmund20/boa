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

-- data OperationSymbol =
-- Plus
-- \| Minus
-- \| Times
-- \| Div
-- \| Mod
-- \| Eq
-- \| Less
-- \| Greater
-- \| In
-- data Value =
-- None
-- \| Boolean Bool
-- \| Number  Integer
-- \| Text    String
-- \| List    [Value]
-- TODO
operate :: OperationSymbol -> Value -> Value -> Either ErrorMessage Value
operate op None _ = Left $ "Operator " ++ show op ++ " with None in left argument."
operate op _ None = Left $ "Operator " ++ show op ++ " with None in right argument."
operate Plus (Number l) (Number r) = Right $ Number (l + r)
operate Minus (Number l) (Number r) = Right $ Number (l - r)
operate Times (Number l) (Number r) = Right $ Number (l * r)
operate Div (Number l) (Number r) = Right $ Number (l `div` r)
operate Mod (Number l) (Number r) = Right $ Number (l `mod` r)
operate Eq (Number l) (Number r) = Right $ Boolean $ l == r
operate Eq (Boolean l) (Boolean r) = Right $ Boolean $ l == r
operate Eq (Text l) (Text r) = Right $ Boolean $ l == r
operate Eq (List l) (List r) = Right $ Boolean $ l == r
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
