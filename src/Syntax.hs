-- Abstract syntax definitions for Boa. Do not modify anything!

module Syntax where

data Value
    = None
    | Boolean Bool
    | Number Integer
    | Text String
    | List [Value]
    deriving (Eq, Show, Read)

data Expression
    = Constant Value
    | Variable VariableName
    | Operation OperationSymbol Expression Expression
    | Not Expression
    | Call FunctionName FunctionInput
    | ListExpression [Expression]
    | ListComprehension Expression [Clause]
    deriving (Eq, Show, Read)

type VariableName = String
type FunctionName = String
type FunctionInput = [Expression]
type FunctionArguments = [Value]

data OperationSymbol
    = Plus
    | Minus
    | Times
    | Div
    | Mod
    | Eq
    | NotEq
    | Less
    | LessEq
    | Greater
    | GreaterEq
    | In
    deriving (Eq, Show, Read)

data Clause
    = For VariableName Expression
    | If Expression
    deriving (Eq, Show, Read)

type Program = [Statement]

data Statement
    = Define VariableName Expression
    | Execute Expression
    deriving (Eq, Show, Read)
