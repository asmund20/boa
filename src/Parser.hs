-- Skeleton file for Boa Parser.

module Parser (ParseError, parseString) where

import Syntax
import Text.ParserCombinators.Parsec

-- add any other other imports you need
-- Program       ::= Statements
program :: Parser Program
program = statements

-- Statements    ::= Statement StatementsEnd
statements :: Parser [Statement]
statements = do
    _ <- many comment
    x <- statement
    xs <- statementsEnd
    _ <- many comment
    return (x : xs)

comment :: Parser ()
comment = spaces >> (char '#') >> many (noneOf "\n\r") >> newline >> return ()

{-
StatementsEnd ::= eps
                | ';' Statements
-}
statementsEnd :: Parser [Statement]
statementsEnd =
    ( do
        _ <- char ';'
        xs <- statements
        return xs
    )
        <|> return []

{-
Statement     ::= ident '=' Expr
                | Expr
-}
statement :: Parser Statement
statement =
    ( do
        i <- identifier
        _ <- char '='
        e <- expr
        return $ Define i e
    )
        <|> ( do
                e <- expr
                return $ Execute e
            )

{-
Expr          ::= ExprPart ExprEnd
-}
expr :: Parser Expression
expr = do
    l <- exprPart
    exprEnd l

{-
ExprEnd       ::= eps
                | Oper Expr
-}
-- 1. The logical-negation operator not. Nesting is allowed, so not not x < 3 parses like not (not (x < 3)).
-- 2. All relational operators (==, etc., including in and not``in). These are all non-associative, i.e., chains like x < y < z are syntactically illegal (unlike in Python).
-- 3. Additive arithmetic operators (+ and -). These are left-associative, e.g., x-y+z parses like (x-y)+z.
-- 4. Multiplicative arithmetic operators (*, //, and \%). These are also left-associative.
-- TODO: parse operator, and based on precedence class and stuff,
exprEnd :: Expression -> Parser Expression
exprEnd e1 =
    ( do
        o <- oper
        e2 <- expr
        -- TODO: Change this so precedence and associativity is as described in the list above by traversing and changing the ast.
        -- Consider doing something else, ask gpt for possible approaches prolly
        return $ Operation Plus e1 e2
    )
        <|> return e1

-- Oper        ::= '+'  | '-'  | '*' | '//' | '%'
-- /| '==' | '!=' | '<' | '<=' | '>' | '>='
oper :: Parser OperationSymbol
oper = do
    _ <- spaces
    c <- (oneOf "+-*/%=!<>")
    case c of
        '+' -> return Plus
        '-' -> return Minus
        '*' -> return Times
        '/' -> (char '/') >> return Div
        '%' -> return Mod
        '=' -> (char '=') >> return Eq
        '!' -> (char '=') >> return NotEq
        '<' -> ((char '=') >> return LessEq) <|> return Less
        '>' -> ((char '=') >> return GreaterEq) <|> return Greater

{-
ExprPart      ::= numConst
                | 'None' | 'True' | 'False'
                | ident ArgsOrEps
                | 'not' Expr
                | '(' Expr ')'
                | '[' ListBody ']'
                | stringConst
-}
exprPart :: Parser Expression
exprPart =
    numConst
        <|> (string "None" >> return (Constant None))
        <|> (string "True" >> return (Constant (Boolean True)))
        <|> (string "False" >> return (Constant (Boolean False)))
        <|> ( do
                i <- identifier
                args <- argsOrEps
                case args of
                    Left args -> return $ Call i args
                    Right _ -> return $ Variable i
            )
        <|> ( do
                _ <- string "not"
                e <- expr
                return $ Not e
            )
        <|> ( do
                _ <- char '('
                e <- expr
                _ <- char ')'
                return e
            )
        <|> ( do
                _ <- char '['
                lb <- listBody
                _ <- char ']'
                return lb
            )
        <|> stringConst

{-
ArgsOrEps     ::= eps
                | '(' Exprz ')'
-}
argsOrEps :: Parser (Either [Expression] ())
argsOrEps =
    ( do
        _ <- char '('
        _ <- spaces
        e <- exprz
        _ <- spaces
        _ <- char ')'
        return $ Left e
    )
        <|> (return $ Right ())

{-
ListBody      ::= eps
                | Expr ListBodyEnd
-}
listBody :: Parser Expression
listBody =
    ( do
        e <- expr
        listBodyEnd e
    )

{-
ListBodyEnd   ::= Exprz
                | ForClause Clausez
-}
listBodyEnd :: Expression -> Parser Expression
listBodyEnd e =
    ( do
        c <- forClause
        cs <- clausez
        return $ ListComprehension e (c : cs)
    )
        <|> ( do
                es <- exprz
                return $ ListExpression (e : es)
            )

{-
ForClause     ::= 'for' ident 'in' Expr
-}
forClause :: Parser Clause
forClause = do
    _ <- string "for"
    ident <- identifier
    _ <- string "in"
    e <- expr
    return $ For ident e

{-
IfClause      ::= 'if' Expr
-}
ifClause :: Parser Clause
ifClause = do
    _ <- string "if"
    e <- expr
    return $ If e

{-
Clausez       ::= eps
                | ForClause Clausez
                | IfClause  Clausez
-}
clausez :: Parser [Clause]
clausez = do
    c <- (forClause <|> ifClause)
    cs <- clausez
    return (c : cs) <|> return []

{-
Exprz         ::= eps
                | Exprs
-}
exprz :: Parser [Expression]
exprz = many expr

{-
Exprs         ::= Expr ExprsEnd
-}
exprs :: Parser [Expression]
exprs = do
    e <- expr
    es <- exprsEnd
    return (e : es)

{-
ExprsEnd      ::= eps
                | ',' Exprs
-}
exprsEnd :: Parser [Expression]
exprsEnd = do
    _ <- char ','
    es <- exprs
    return es

{-
ident         ::= (see text)
-}
identifier :: Parser String
identifier = do
    x <- letter <|> (char '_')
    xs <- many $ letter <|> digit <|> (char '_')
    return (x : xs)

{-
numConst      ::= (see text)
-}
numConst :: Parser Expression
numConst = do
    n <- wholeNumber
    return $ Constant $ Number n

wholeNumber :: Parser Integer
wholeNumber =
    naturalNumber <|> do
        _ <- char '-'
        n <- naturalNumber
        return $ -n

naturalNumber :: Parser Integer
naturalNumber = do
    leading <- oneOf "123456789"
    trailing <- many digit
    return $ read (leading : trailing)

{-
stringConst   ::= (see text)
-}
stringConst :: Parser Expression
stringConst = do
    _ <- char '\''
    s <- many $ (noneOf "'\\" <|> escapedBackSlash <|> escapedSingleQuote)
    _ <- char '\''
    return $ Constant $ Text s

escapedBackSlash :: Parser Char
escapedBackSlash = do
    _ <- char '\\'
    _ <- char '\\'
    return '\\'

escapedSingleQuote :: Parser Char
escapedSingleQuote = do
    _ <- char '\\'
    _ <- char '\''
    return '\''

parseString :: String -> Either ParseError Program
parseString = parse (program <* eof) "input"
