-- Skeleton file for Boa Interpreter. Edit only definitions with 'undefined'

module Interpreter where

import Syntax
import Control.Monad

type Output      = [String]

type ErrorMessage = String
data RuntimeError =
    UnboundVariable VariableName
  | BadFunction     FunctionName
  | BadArgument     ErrorMessage
  deriving (Eq, Show)

type Environment = [(VariableName, Value)]
type Runtime a   = Environment -> (Either RuntimeError a, Output)

newtype Boa a = Boa {run :: Runtime a}

-- Monad instance.
instance Monad Boa where
  return = undefined
  (>>=)  = undefined

-- Freebies.
instance Functor Boa where
  fmap = liftM
instance Applicative Boa where
  pure = return; (<*>) = ap

-- Operations of the Boa monad
abort :: RuntimeError -> Boa a
abort = undefined

look :: VariableName -> Boa Value
look = undefined

bind :: VariableName -> Value -> (Boa a -> Boa a)
bind = undefined

output :: String -> Boa ()
output = undefined

-- Helper functions for interpreter
truthy :: Value -> Bool
truthy = undefined

operate :: OperationSymbol -> Value -> Value -> Either ErrorMessage Value
operate = undefined

apply :: FunctionName -> FunctionArguments -> Boa Value
apply = undefined

-- Main functions of interpreter
eval :: Expression -> Boa Value
eval (Operation op e1 e2) =
  do v1 <- eval e1
     v2 <- eval e2
     case operate op v1 v2 of
       (Right value) -> return value
       (Left  err  ) -> abort $ BadArgument err
eval _ = undefined

exec :: Program -> Boa ()
exec [                       ] = return ()
exec (Define x body : program) =
  do v <- eval body
     bind x v (exec program)
exec (Execute thing : program) =
  do eval thing
     exec program

execute :: Program -> (Output, Maybe RuntimeError)
execute = undefined
