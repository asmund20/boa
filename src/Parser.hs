module Parser (ParseError, parseString) where

import Control.Monad
import Data.List.NonEmpty (cons)
import Data.Maybe
import Syntax
import Text.Parsec.Char (endOfLine)
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
statement =
    between
        (skipMany (skipMany1 space <|> comment))
        (skipMany (skipMany1 space <|> comment))
        ((Execute <$> notExpr) <|> binExprOrAssign)
  where
    binExprOrAssign :: Parser Statement
    binExprOrAssign = do
        e <- relPart
        case e of
            Variable i -> execOrAssign i
            _ -> Execute <$> ((rightPart e) <|> (return e))
    rightPart :: Expression -> Parser Expression
    rightPart e = relOper <*> return e <*> relPart
    -- \| Here, assign statement and equality-expressions are left-factorized by trying to parse "==" and '=' after an identifier
    execOrAssign :: VariableName -> Parser Statement
    execOrAssign i =
        (char '=' >> spaces >> assign i) <|> Execute
            <$> ((rightPart (Variable i)) <|> return (Variable i))
    assign :: VariableName -> Parser Statement
    assign i =
        (char '=' >> spaces >> Execute <$> (Operation Eq (Variable i)) <$> relPart)
            <|> (Define i <$> expr)

-- |  Expr          ::= 'not' Expr | RelPart [ RelOper Expr ]
expr :: Parser Expression
expr = notExpr <|> binExpr
  where
    binExpr :: Parser Expression
    binExpr = do
        e <- relPart
        (relOper <*> return e <*> relPart) <|> return e

notExpr :: Parser Expression
notExpr = (keyWord "not") >> Not <$> expr

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
exprLiteral =
    ( notExpr
        <|> numConst
        <|> stringConst
        <|> (keyWord "None" >> return (Constant None))
        <|> (keyWord "True" >> return (Constant $ Boolean True))
        <|> (keyWord "False" >> return (Constant $ Boolean False))
        <|> ident
        <|> parenExpr
        <|> list
    )
        <* spaces
  where
    ident :: Parser Expression
    ident = do
        s <- identifier
        (Call s) <$> params <|> return (Variable s)
    params :: Parser [Expression]
    params =
        between
            (char '(' <* spaces)
            (char ')' <* spaces)
            (sepBy expr (consumeSpaces $ char ','))
    parenExpr :: Parser Expression
    parenExpr = between (char '(' <* spaces) (char ')' <* spaces) expr
    list :: Parser Expression
    list = between (char '[' <* spaces) (char ']' <* spaces) listBody

consumeSpaces :: Parser a -> Parser a
consumeSpaces p = between spaces spaces p

keyWord :: String -> Parser ()
keyWord s = try (string s >> notFollowedBy identifierBodyPart) *> spaces

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
relOper :: Parser (Expression -> Expression -> Expression)
relOper =
    consumeSpaces (eq <|> notEq <|> lessEq <|> greaterEq <|> inOper <|> notInOper)
  where
    eq :: Parser (Expression -> Expression -> Expression)
    eq = string "==" >> return (Operation Eq)
    notEq :: Parser (Expression -> Expression -> Expression)
    notEq = string "!=" >> return (notOper Eq)
    lessEq :: Parser (Expression -> Expression -> Expression)
    lessEq =
        char '<'
            >> ( (char '=' >> return (notOper Greater))
                    <|> return
                        (Operation Less)
               )
    greaterEq :: Parser (Expression -> Expression -> Expression)
    greaterEq =
        char '>'
            >> ( (char '=' >> return (notOper Less))
                    <|> return
                        (Operation Greater)
               )
    inOper :: Parser (Expression -> Expression -> Expression)
    inOper = keyWord "in" >> return (Operation In)
    notInOper :: Parser (Expression -> Expression -> Expression)
    notInOper =
        keyWord "not"
            >> spaces
            >> string "in"
            >> return (notOper In)
    notOper :: OperationSymbol -> (Expression -> Expression -> Expression)
    notOper o = (\l r -> Not (Operation o l r))

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
    clauses =
        (ListComprehension e)
            <$> ( pure (:)
                    <*> (consumeSpaces forClause)
                    <*> many (consumeSpaces (forClause <|> ifClause))
                )
    commaSepExprs :: Parser Expression
    commaSepExprs =
        ListExpression
            <$> ( ( char ','
                        >> spaces
                        >> pure (:) <*> pure e <*> sepBy1 expr (consumeSpaces $ char ',')
                  )
                    <|> return [e]
                )

-- | ForClause     ::= 'for' ident 'in' Expr
forClause :: Parser Clause
forClause = do
    _ <- consumeSpaces $ string "for"
    ident <- identifier
    _ <- string "in" <* spaces
    For ident <$> expr

-- | IfClause      ::= 'if' Expr
ifClause :: Parser Clause
ifClause = string "if" >> spaces >> If <$> expr

identifier :: Parser String
identifier = pure (:) <*> (letter <|> char '_') <*> many identifierBodyPart <* spaces

identifierBodyPart :: Parser Char
identifierBodyPart = letter <|> digit <|> char '_'

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
    naturalNumber = (char '0' >> notFollowedBy digit >> return 0) <|> read <$> many1 digit

stringConst :: Parser Expression
stringConst = Constant . Text <$> between (char '\'') (char '\'') body
  where
    body :: Parser String
    body = catMaybes <$> (many (allowedChar <|> backSlashOrSingleQuote))
    allowedChar :: Parser (Maybe Char)
    allowedChar = Just <$> (noneOf "'\\\n\r'")
    backSlashOrSingleQuote :: Parser (Maybe Char)
    backSlashOrSingleQuote =
        char '\\'
            >> ( Just
                    <$> char '\\'
                        <|> Just
                    <$> char '\''
                        <|> (char 'n' >> return (Just '\n'))
                        <|> (endOfLine >> return Nothing)
               )

parseString :: String -> Either ParseError Program
parseString = parse (program <* eof) "input"
