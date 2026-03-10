module Parser (ParseError, parseString) where

import Control.Monad
import Data.List.NonEmpty (cons)
import Syntax
import Text.ParserCombinators.Parsec

-- |  Program       ::= Statements
program :: Parser Program
program = statements

-- |  Statements    ::= Statement [ ; Statements ]
statements :: Parser [Statement]
statements = sepBy1 statement (char ';')

comment :: Parser ()
comment = void (char '#' >> manyTill anyChar newline)

-- |  Statement     ::= ident '=' Expr | Expr
statement :: Parser Statement
statement = skipMany (skipMany1 space <|> comment) >> ((Execute <$> notExpr) <|> binExprOrAssign)
  where
    binExprOrAssign :: Parser Statement
    binExprOrAssign = do
        e <- relPart
        case e of
            Variable i -> execOrAssign i
            _ -> Execute <$> binExprRightPart Nothing e

    -- \| Here, assign statement and equality-expressions are left-factorized by trying to parse "==" and '=' after an identifier
    execOrAssign :: VariableName -> Parser Statement
    execOrAssign i = do
        m <- optionMaybe $ char '='
        case m of
            Nothing -> Execute <$> binExprRightPart Nothing (Variable i)
            Just _ -> do
                m' <- optionMaybe $ char '='
                case m' of
                    Nothing -> Define i <$> expr
                    Just _ -> Execute <$> binExprRightPart (Just Eq) (Variable i)

binExprRightPart :: Maybe OperationSymbol -> Expression -> Parser Expression
binExprRightPart om e1 = case om of
    Nothing -> do
        m <- optionMaybe relOper
        case m of
            Nothing -> return e1
            Just o -> binExpSecondExp e1 o
    Just o -> binExpSecondExp e1 o
  where
    binExpSecondExp :: Expression -> OperationSymbol -> Parser Expression
    binExpSecondExp e1 o = Operation o e1 <$> relPart

-- |  Expr          ::= 'not' Expr | RelPart [ RelOper Expr ]
expr :: Parser Expression
expr = consumeSpaces (notExpr <|> binExpr)
  where
    binExpr :: Parser Expression
    binExpr = do
        e <- relPart
        m <- optionMaybe relOper
        case m of
            Nothing -> return e
            Just o -> binExprRightPart (Just o) e

notExpr :: Parser Expression
notExpr = do
    _ <- try $ string "not"
    Not <$> expr

-- |  RelPart       ::= Term [ AddOper Expr ]
relPart :: Parser Expression
relPart = consumeSpaces (notExpr <|> chainl1 term addOper)

-- | Term          ::= ExprLiteral [ MultOper Expr ]
term :: Parser Expression
term = consumeSpaces (notExpr <|> chainl1 exprLiteral multOper)

{- |
ExprLiteral   ::= numConst
                | stringConst
                | 'None' | 'True' | 'False'
                | ident [ '(' Exprz ')' ]
                | '(' Expr ')'
                | '[' [Expr ListBodyEnd] ']'
-}
exprLiteral :: Parser Expression
exprLiteral = consumeSpaces (notExpr <|> numConst <|> stringConst <|> none <|> true <|> false <|> ident <|> parenExpr <|> list)
  where
    none :: Parser Expression
    none = string "None" >> return (Constant None)
    true :: Parser Expression
    true = string "True" >> return (Constant $ Boolean True)
    false :: Parser Expression
    false = string "False" >> return (Constant $ Boolean False)
    ident :: Parser Expression
    ident = do
        s <- identifier
        m <- optionMaybe params
        case m of
            Nothing -> return $ Variable s
            Just ps -> return $ Call s ps
    params :: Parser [Expression]
    params = between (char '(') (char ')') (sepBy expr (char ','))
    parenExpr :: Parser Expression
    parenExpr = between (char '(') (char ')') expr
    list :: Parser Expression
    list = between (char '[') (char ']') listBody

consumeSpaces :: Parser a -> Parser a
consumeSpaces p = between spaces spaces p

-- |  MultOper      ::= '*'  | '//' | '%  |
multOper :: Parser (Expression -> Expression -> Expression)
multOper = consumeSpaces (times <|> div <|> mod)
  where
    times :: Parser (Expression -> Expression -> Expression)
    times = char '*' >> return (Operation Times)
    div :: Parser (Expression -> Expression -> Expression)
    div = string "//" >> return (Operation Div)
    mod :: Parser (Expression -> Expression -> Expression)
    mod = char '%' >> return (Operation Mod)

-- |  AddOper       ::= '+'  | '-'
addOper :: Parser (Expression -> Expression -> Expression)
addOper = consumeSpaces (plus <|> minus)
  where
    plus :: Parser (Expression -> Expression -> Expression)
    plus = char '+' >> return (Operation Plus)
    minus :: Parser (Expression -> Expression -> Expression)
    minus = char '-' >> return (Operation Minus)

-- |  RelOper       ::= '==' | '!=' | '<' [ '=' ] | '>' [ '=' ] | 'in' | 'not' 'in'
relOper :: Parser OperationSymbol
relOper = consumeSpaces (eq <|> notEq <|> lessEq <|> greaterEq <|> inOper <|> notInOper)
  where
    eq :: Parser OperationSymbol
    eq = string "==" >> return Eq
    notEq :: Parser OperationSymbol
    notEq = string "!=" >> return NotEq
    lessEq :: Parser OperationSymbol
    lessEq = char '<' >> ((char '=' >> return LessEq) <|> return Less)
    greaterEq :: Parser OperationSymbol
    greaterEq = char '>' >> ((char '=' >> return GreaterEq) <|> return Greater)
    inOper :: Parser OperationSymbol
    inOper = try $ string "in" >> return In
    notInOper :: Parser OperationSymbol
    notInOper = string "not" >> spaces >> string "in" >> return NotIn

-- | ListBody      ::= Expr ListBodyEnd
listBody :: Parser Expression
listBody = do
    m <- optionMaybe expr
    case m of
        Nothing -> return $ ListExpression []
        Just e -> listBodyEnd e

{-
ListBodyEnd   ::= Exprz
                | ForClause Clausez
-}
listBodyEnd :: Expression -> Parser Expression
listBodyEnd e = clauses <|> commaSepExprs
  where
    clauses :: Parser Expression
    clauses = do
        c <- consumeSpaces forClause
        cs <- many $ consumeSpaces (forClause <|> ifClause)
        return $ ListComprehension e (c : cs)
    commaSepExprs :: Parser Expression
    commaSepExprs = do
        m <- optionMaybe $ char ','
        case m of
            Nothing -> return $ ListExpression [e]
            Just _ -> do
                es <- sepBy expr (char ',')
                return $ ListExpression (e : es)

-- | ForClause     ::= 'for' ident 'in' Expr
forClause :: Parser Clause
forClause = do
    _ <- consumeSpaces $ string "for"
    ident <- identifier
    _ <- string "in"
    For ident <$> expr

-- | IfClause      ::= 'if' Expr
ifClause :: Parser Clause
ifClause = string "if" >> If <$> expr

identifier :: Parser String
identifier = consumeSpaces $ do
    x <- letter <|> char '_'
    xs <- many $ letter <|> digit <|> char '_'
    return (x : xs)

numConst :: Parser Expression
numConst = Constant . Number <$> wholeNumber
  where
    wholeNumber :: Parser Integer
    wholeNumber =
        naturalNumber <|> do
            _ <- char '-'
            n <- naturalNumber
            return $ -n
    naturalNumber :: Parser Integer
    naturalNumber = read <$> many1 digit

stringConst :: Parser Expression
stringConst = Constant . Text <$> between (char '\'') (char '\'') (many (noneOf "'\\" <|> backSlashOrSingleQuote))
  where
    backSlashOrSingleQuote :: Parser Char
    backSlashOrSingleQuote = char '\\' >> oneOf "\\\'"

parseString :: String -> Either ParseError Program
parseString = parse (program <* eof) "input"
