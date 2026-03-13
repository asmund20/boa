module ParserTests where

import Data.Char (chr)
import qualified Data.Map as Map
import Interpreter
import Parser
import Syntax
import Test.Tasty.QuickCheck

newtype BoaProgram = P String deriving (Show, Eq)

data BoaType = TNone | TBoolean | TNumber | TText | TList | TAny deriving (Eq, Show)

type Decls = Map.Map String BoaType

first :: (String, BoaType) -> String
first (s, _) = s

declsOfType :: BoaType -> Decls -> [String]
declsOfType TAny decls = Map.keys decls
declsOfType t decls = Map.keys $ Map.filter (== t) decls

emptyDecls :: Decls
emptyDecls = Map.empty

instance Arbitrary BoaProgram where
    arbitrary = do
        (s, _) <- sized $ sizedProgram emptyDecls
        return $ P s

genType :: Gen BoaType
genType = oneof [return TNone, return TBoolean, return TNumber, return TText, return TList]

sizedProgram :: Decls -> Int -> Gen (String, Decls)
sizedProgram decls 0 = oneof [assignment, expression]
  where
    assignment :: Gen (String, Decls)
    assignment = do
        i <- identGen
        s1 <- genSpaces
        s2 <- genSpaces
        (e, _) <- expression
        t <- genType
        return (i ++ s1 ++ '=' : s2 ++ e, Map.insert i t decls)
    expression :: Gen (String, Decls)
    expression = do
        e <- sized $ sizedExpression decls
        return (e, emptyDecls)
    letterGen :: Gen Char
    letterGen = elements $ ['a' .. 'z'] ++ ['A' .. 'Z']
    identStartGen :: Gen Char
    identStartGen =
        frequency
            [ (3, letterGen)
            , (1, return '_')
            ]
    identBodypartGen :: Gen Char
    identBodypartGen =
        frequency
            [ (5, letterGen)
            , (1, return '_')
            , (1, elements ['0' .. '9'])
            ]
    identGen :: Gen String
    identGen = do
        c <- identStartGen
        cs <- listOf identBodypartGen
        return (c : cs)
sizedProgram decls n = do
    (s1, decls') <- sizedProgram decls 0
    (ss, _) <- sizedProgram decls' (n - 1)
    spaces1 <- genSpaces
    spaces2 <- genSpaces
    return (s1 ++ spaces1 ++ ';' : spaces2 ++ ss, emptyDecls)

sizedExpression :: Decls -> Int -> Gen String
sizedExpression decls 0 = exprLiteral decls
sizedExpression decls n = typedSizedExpression TAny decls n

-- data BoaType = TNone | TBoolean | TNumber | TText | TList | TAny
typedSizedExpression :: BoaType -> Decls -> Int -> Gen String
typedSizedExpression TNone _ 0 = return "None"
typedSizedExpression TBoolean _ 0 = oneof [return "True", return "False"]
typedSizedExpression TNumber _ 0 = show <$> arbitrarySizedNatural
typedSizedExpression TText _ 0 = sized sizedStringConst
typedSizedExpression TList decls 0 = return "[]"
typedSizedExpression TAny decls 0 = exprLiteral decls
typedSizedExpression TAny decls n = do
    t <- genType
    typedSizedExpression t decls n
typedSizedExpression TNone decls n = frequency [(1, return "None"), (2, printCall)]
  where
    printCall = do
        partitioning <- choose (1, 10)
        let expLength = n `div` partitioning
        let listLength = n - expLength
        csv <- genCsvExprs TAny listLength decls expLength
        s1 <- genSpaces
        s2 <- genSpaces
        s3 <- genSpaces
        return $ "print" ++ s1 ++ "(" ++ s2 ++ csv ++ s3 ++ ")"
typedSizedExpression TList decls n = oneof [listExp, listComprehension, rangeCall]
  where
    listExp = do
        partitioning <- choose (1, 10)
        let expLength = n `div` partitioning
        let listLength = n - expLength
        csv <- genCsvExprs TAny listLength decls expLength
        s1 <- genSpaces
        s2 <- genSpaces
        return $ '[' : s1 ++ csv ++ s2 ++ "]"
    listComprehension = do
        -- I want to do this by first generating the clauses.
        -- The for clauses all generate a valid identifier for a
        -- TNumber, these should be returned and used as the
        -- environment, probably with a selection of the Number
        -- identifiers that already exist for creating the expression
        partitioning <- choose (1, 10)
        let expLength = n `div` partitioning
        let clauseLength = n - expLength
        (e, c) <- genForClause -- unsure what type to use here
        cs <- genClauses clauseLength expLength
        s1 <- genSpaces
        s2 <- genSpaces
        s3 <- genSpaces
        s4 <- genSpaces
        return $ '[' : s1 ++ e ++ s2 ++ c ++ s3 ++ cs ++ s4 ++ "]"
    rangeCall = do
        params <- choose (1, 3)
        csv <- genCsvExprs TNumber params decls (n `div` params)
        s1 <- genSpaces
        s2 <- genSpaces
        return $ "range" ++ '(' : s1 ++ csv ++ s2 ++ ")"
    genForClause = undefined
    genIfClause = undefined
    genClauses clauseLength expLength = do
        c <- oneof [genForClause genIfClause]
        cs <- genClauses (clauseLength - 1) expLength
        s1 <- genSpaces
        return $ c ++ ' ' : s1 ++ cs
typedSizedExpression t decls n = oneof [binaryExp, notExp, parenExp, sizedExpression decls 0]
  where
    binaryExp = do
        -- TODO: This must probably be a bit more sophisticated, Bools can be multiplicated with anything, resulting type is the non-bool or bool
        -- anything can be compared size-wise except None, False == 0 and True == 1, anything can be compared for equality
        e1 <- typedSizedExpression t decls (n `div` 2)
        e2 <- typedSizedExpression t decls (n `div` 2)
        s1 <- genSpaces
        s2 <- genSpaces
        op <- genOperator t
        return $ e1 ++ s1 ++ op ++ s2 ++ e2
    notExp = do
        e <- typedSizedExpression t decls (n - 1)
        s <- genSpaces
        return $ "not" ++ s ++ e
    parenExp = do
        e <- typedSizedExpression t decls (n - 1)
        s1 <- genSpaces
        s2 <- genSpaces
        return $ '(' : s1 ++ e ++ s2 ++ ")"

genCsvExprs :: BoaType -> Int -> Decls -> Int -> Gen String
genCsvExprs t 0 decls exprSize = typedSizedExpression t decls exprSize
genCsvExprs t n decls exprSize = do
    e <- typedSizedExpression t decls exprSize
    es <- genCsvExprs t (n - 1) decls exprSize
    s1 <- genSpaces
    s2 <- genSpaces
    return $ e ++ s1 ++ ',' : s2 ++ es

genOperator :: BoaType -> Gen String
genOperator = undefined

typeGen :: Gen BoaType
typeGen = oneof [return TNone, return TBoolean, return TNumber, return TText, return TList]

exprLiteral :: Decls -> Gen String
exprLiteral decls = oneof [numConst, sized sizedStringConst, none, true, false, ident]
  where
    numConst = show <$> arbitrarySizedNatural
    none = return "None"
    true = return "True"
    false = return "True"
    ident = elements $ map first $ Map.toList decls

sizedStringConst :: Int -> Gen String
sizedStringConst size = do
    n <- choose (0, size)
    body <-
        concat
            <$> vectorOf
                n
                ( frequency
                    [ (9, elements $ map charToString $ filter (`notElem` ['\'', '\\']) $ map chr [32 .. 126]) -- printables except ' and \
                    , (1, oneof [return "\\'", return "\\\\", return "\\n", return "\\\n", return "\\\r\n"])
                    ]
                )
    return $ '\'' : body ++ "'"
  where
    charToString c = [c]

genEndOfline :: Gen String
genEndOfline =
    oneof
        [return "\r\n", return "\n"]

genSpace :: Gen String
genSpace =
    frequency
        [ (1, genEndOfline)
        , (3, return " ")
        ]

genSpaces :: Gen String
genSpaces = do
    n <- choose (0, 6)
    s <- vectorOf n genSpace
    return $ concat s
