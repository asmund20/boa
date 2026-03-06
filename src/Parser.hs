-- |  Skeleton file for Boa Parser.
module Parser (ParseError, parseString) where

import Data.List.NonEmpty (cons)
import Syntax
import Text.ParserCombinators.Parsec

{- |  add any other other imports you need
|  Program       ::= Statements
-}
program :: Parser Program
program = statements

-- |  Statements    ::= Statement [ ; Statements ]
statements :: Parser [Statement]
statements = sepBy1 statement (char ';')

comment :: Parser ()
comment = spaces >> char '#' >> many (noneOf "\n\r") >> newline >> return ()

charSpaces :: Char -> Parser Char
charSpaces c = spaces >> char 'c' >> spaces >> return c

-- |  Statement     ::= ident '=' Expr | Expr
statement :: Parser Statement
statement = do
    _ <- many comment
    s <- (assign <|> expression)
    _ <- many comment
    return s
  where
    assign = do
        i <- identifier
        _ <- charSpaces '='
        e <- expr
        return $ Define i e
    expression = do
        e <- expr
        return $ Execute e

notExpr :: Parser Expression
notExpr = do
    _ <- string "not"
    e <- expr
    return $ Not e

-- |  Expr          ::= 'not' Expr | RelPart [ RelOper Expr ]
expr :: Parser Expression
expr = consumeSpaces (notExpr <|> binExpr)
  where
    binExpr = do
        e <- relPart
        m <- optionMaybe relOper
        case m of
            Nothing -> return e
            Just o -> rightExpr o e
    rightExpr os e1 = do
        e2 <- expr
        return $ Operation os e1 e2

-- |  RelPart       ::= Term [ AddOper Expr ]
relPart :: Parser Expression
relPart = notExpr <|> chainl1 term addOper

-- | Term          ::= ExprLiteral [ MultOper Expr ]
term :: Parser Expression
term = notExpr <|> chainl1 exprLiteral multOper

{- |
ExprLiteral   ::= numConst
                | stringConst
                | 'None' | 'True' | 'False'
                | ident [ '(' Exprz ')' ]
                | '(' Expr ')'
                | '[' [Expr ListBodyEnd] ']'
-}
exprLiteral :: Parser Expression
exprLiteral = notExpr <|> numConst <|> stringConst <|> none <|> true <|> false <|> ident <|> parenExpr <|> list
  where
    none = string "None" >> return (Constant None)
    true = string "True" >> return (Constant $ Boolean True)
    false = string "False" >> return (Constant $ Boolean False)
    ident = do
        s <- identifier
        m <- optionMaybe params
        case m of
            Nothing -> return $ Variable s
            Just ps -> return $ Call s ps
    params = do
        _ <- char '('
        es <- sepBy expr (char ',')
        _ <- char ')'
        return es
    parenExpr = char '(' >> expr <* char ')'
    list = do
        _ <- char '['
        listBody

{-
        <|> ( do
                _ <- char '['
                lb <- optionMaybe listBody
                _ <- char ']'
                case lb of
                    Nothing -> return $ ListExpression []
                    Just list -> return list
            )
        <|> stringConst
        -}

consumeSpaces :: Parser a -> Parser a
consumeSpaces p = do
    _ <- spaces
    r <- p
    _ <- spaces
    return r

-- |  MultOper      ::= '*'  | '//' | '%  |
multOper :: Parser (Expression -> Expression -> Expression)
multOper = consumeSpaces times <|> consumeSpaces div <|> consumeSpaces mod
  where
    times = char '*' >> return (\e1 e2 -> Operation Times e1 e2)
    div = string "//" >> return (\e1 e2 -> Operation Div e1 e2)
    mod = char '%' >> return (\e1 e2 -> Operation Mod e1 e2)

-- |  AddOper       ::= '+'  | '-'
addOper :: Parser (Expression -> Expression -> Expression)
addOper = consumeSpaces plus <|> consumeSpaces minus
  where
    plus = char '+' >> return (\e1 e2 -> Operation Plus e1 e2)
    minus = char '-' >> return (\e1 e2 -> Operation Minus e1 e2)

-- |  RelOper       ::= '==' | '!=' | '<' [ '=' ] | '>' [ '=' ] | 'in' | 'not' 'in'
relOper :: Parser OperationSymbol
relOper = consumeSpaces eq <|> consumeSpaces notEq <|> consumeSpaces lessEq <|> consumeSpaces greaterEq
  where
    eq = string "==" >> return Eq
    notEq = string "!=" >> return NotEq
    lessEq = char '<' >> ((char '=' >> return LessEq) <|> return Less)
    greaterEq = char '>' >> ((char '=' >> return GreaterEq) <|> return Greater)

-- | ListBody      ::= Expr ListBodyEnd
listBody :: Parser Expression
listBody =
    ( do
        m <- optionMaybe expr
        case m of
            Nothing -> return $ ListExpression []
            Just e -> listBodyEnd e
    )

{-
ListBodyEnd   ::= Exprz
                | ForClause Clausez
-}
listBodyEnd :: Expression -> Parser Expression
listBodyEnd e = clauses <|> cse
  where
    clauses = do
        c <- forClause
        cs <- many (forClause <|> ifClause)
        return $ ListComprehension e (c : cs)
    cse = do
        es <- sepBy expr (char ',')
        return $ ListExpression (e : es)

-- | ForClause     ::= 'for' ident 'in' Expr
forClause :: Parser Clause
forClause = do
    _ <- string "for"
    ident <- identifier
    _ <- string "in"
    e <- expr
    return $ For ident e

-- | IfClause      ::= 'if' Expr
ifClause :: Parser Clause
ifClause = do
    _ <- string "if"
    e <- expr
    return $ If e

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
    s <- many1 digit
    return $ read s

{-
stringConst   ::= (see text)
-}
stringConst :: Parser Expression
stringConst = do
    _ <- char '\''
    s <- many $ (noneOf "'\\" <|> escapedBackSlash <|> escapedSingleQuote)
    _ <- char '\''
    return $ Constant $ Text s
  where
    escapedBackSlash = do
        _ <- char '\\'
        _ <- char '\\'
        return '\\'
    escapedSingleQuote = do
        _ <- char '\\'
        _ <- char '\''
        return '\''

parseString :: String -> Either ParseError Program
parseString = parse (program <* eof) "input"
