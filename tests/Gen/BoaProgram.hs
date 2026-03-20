module Gen.BoaProgram where

import Data.Char (chr)
import qualified Data.Map as Map
import Interpreter
import Parser
import Syntax
import Test.Tasty.QuickCheck

{-
 - TODO
 - Python: 'not' after an operator must be parenthesized-}

newtype BoaProgram = P String deriving (Eq)

instance Show BoaProgram where
    show (P p) = p

data BoaType = TNone | TBoolean | TNumber | TText | TList | TAny
    deriving (Eq, Show)

type Decls = Map.Map String BoaType

first :: (String, BoaType) -> String
first (s, _) = s

declsOfType :: BoaType -> Decls -> [String]
declsOfType TAny decls = Map.keys decls
declsOfType t decls = Map.keys $ Map.filter (== t) decls

emptyDecls :: Decls
emptyDecls = Map.empty

instance Arbitrary BoaProgram where
    arbitrary = resize 15 $ do
        (s, _) <- sized $ sizedProgram emptyDecls
        return $ P s

genType :: Gen BoaType
genType =
    oneof
        [ return TNone
        , return TBoolean
        , return TNumber
        , return TText
        , return TList
        ]

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
sizedProgram decls n = do
    (s1, decls') <- sizedProgram decls 0
    (ss, _) <- sizedProgram decls' (n - 1)
    spaces1 <- genSpaces
    spaces2 <- genWhiteSpaces
    return (s1 ++ spaces1 ++ ';' : spaces2 ++ ss, emptyDecls)

identGen :: Gen String
identGen = do
    c <- identStartGen
    cs <- listOf identBodypartGen
    return (c : cs)
  where
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

sizedExpression :: Decls -> Int -> Gen String
sizedExpression decls 0 = do
    (e, _) <- exprLiteral decls
    return e
sizedExpression decls n = do
    (e, _) <- typedSizedExpression TAny decls n
    return e

-- TODO: All typedSizedExpression should have the possibility of choosing an identifier with the correct type
-- data BoaType = TNone | TBoolean | TNumber | TText | TList | TAny
typedSizedExpression ::
    BoaType -> Decls -> Int -> Gen (String, BoaType)
typedSizedExpression TNone _ 0 = return ("None", TNone)
typedSizedExpression TBoolean _ 0 =
    oneof
        [ return ("True", TBoolean)
        , return ("False", TBoolean)
        ]
typedSizedExpression TNumber _ 0 = do
    n <- show <$> arbitrarySizedNatural
    return $ (n, TNumber)
typedSizedExpression TText _ 0 = do
    s <- sized sizedStringConst
    return $ (s, TText)
typedSizedExpression TList decls 0 = return ("[]", TAny)
typedSizedExpression TAny decls 0 = exprLiteral decls
typedSizedExpression TAny decls n = do
    t <- genType
    typedSizedExpression t decls n
typedSizedExpression TNone decls n =
    frequency
        [ (1, return ("None", TNone))
        , (2, printCall)
        ]
  where
    printCall = do
        partitioning <- choose (1, 10)
        let expLength = n `div` partitioning
        let listLength = n - expLength
        (csv, _) <- genCsvExprs TAny listLength decls expLength
        s1 <- genSpaces
        s2 <- genWhiteSpaces
        s3 <- genWhiteSpaces
        return ("print" ++ s1 ++ "(" ++ s2 ++ csv ++ s3 ++ ")", TNone)
typedSizedExpression TList decls n = oneof [listExp, listComprehension, rangeCall]
  where
    listExp = do
        partitioning <- choose (1, 10)
        let expLength = n `div` partitioning
        let listLength = n - expLength
        (csv, t) <- genCsvExprs TAny listLength decls expLength
        s1 <- genWhiteSpaces
        s2 <- genWhiteSpaces
        return ('[' : s1 ++ csv ++ s2 ++ "]", t)
    listComprehension = do
        partitioning <- choose (1, 10)
        let expLength = n `div` partitioning
        let clauseLength = n - expLength
        (c, decls'') <- genForClause expLength decls
        (cs, decls') <- genClauses clauseLength expLength decls''
        (e, t) <- typedSizedExpression TAny decls' expLength
        s1 <- genWhiteSpaces
        s2 <- genWhiteSpaces1
        s3 <- genWhiteSpaces
        s4 <- genWhiteSpaces
        return ('[' : s1 ++ e ++ s2 ++ c ++ s3 ++ cs ++ s4 ++ "]", t)
    rangeCall = do
        params <- choose (1, 3)
        (csv, _) <- genCsvExprs TNumber params decls (n `div` params)
        s1 <- genSpaces
        s2 <- genWhiteSpaces
        s3 <- genWhiteSpaces
        return ("range" ++ s1 ++ '(' : s2 ++ csv ++ s3 ++ ")", TNumber)
    genForClause :: Int -> Decls -> Gen (String, Decls)
    genForClause expLength d = do
        i <- identGen
        (l, t) <- typedSizedExpression TList d expLength
        s1 <- genWhiteSpaces
        s2 <- genWhiteSpaces
        s3 <- genWhiteSpaces
        return ("for " ++ s1 ++ i ++ s2 ++ "in " ++ s3 ++ l, Map.insert i t d)
    genIfClause :: Int -> Decls -> Gen (String, Decls)
    genIfClause expLength decls = do
        (e, _) <-
            oneof
                [ typedSizedExpression TAny decls expLength
                , typedSizedExpression TBoolean decls expLength
                ]
        s1 <- genWhiteSpaces1
        return $ ("if" ++ s1 ++ e, decls)
    genClauses :: Int -> Int -> Decls -> Gen (String, Decls)
    genClauses 0 expLength decls = return ("", decls)
    genClauses clauseLength expLength decls = do
        (c, decls) <-
            oneof [genForClause expLength decls, genIfClause expLength decls]
        (cs, decls) <- genClauses (clauseLength - 1) expLength decls
        s1 <- genWhiteSpaces1
        return (c ++ s1 ++ cs, decls)
typedSizedExpression TNumber decls n =
    oneof
        [ binaryExp TNumber decls n
        , parenExp TNumber decls n
        , typedSizedExpression TNumber decls 0
        ]
typedSizedExpression t decls n =
    oneof
        [ binaryExp t decls n
        , notExp t decls n
        , parenExp t decls n
        , typedSizedExpression t decls 0
        ]
binaryExp :: BoaType -> Decls -> Int -> Gen (String, BoaType)
binaryExp t decls n = do
    (o, tl, tr) <- genOperator t
    (el, _) <- typedSizedExpression tl decls (n `div` 2)
    (er, _) <- typedSizedExpression tr decls (n `div` 2)
    s1 <- genSpaces
    s2 <- genSpaces
    return (el ++ s1 ++ o ++ s2 ++ er, t)
notExp :: BoaType -> Decls -> Int -> Gen (String, BoaType)
notExp t decls n = do
    (e, _) <- typedSizedExpression t decls (n - 1)
    s <- genSpaces1
    return ("not" ++ s ++ e, TBoolean)
parenExp :: BoaType -> Decls -> Int -> Gen (String, BoaType)
parenExp t decls n = do
    (e, _) <- typedSizedExpression t decls (n - 1)
    s1 <- genWhiteSpaces
    s2 <- genWhiteSpaces
    return ('(' : s1 ++ e ++ s2 ++ ")", t)

genCsvExprs :: BoaType -> Int -> Decls -> Int -> Gen (String, BoaType)
genCsvExprs t 0 decls exprSize = return ("", TAny)
genCsvExprs t 1 decls exprSize = typedSizedExpression t decls exprSize
genCsvExprs TAny n decls exprSize = do
    t <- genType
    genCsvExprs t n decls exprSize
genCsvExprs t n decls exprSize = do
    (e, _) <- typedSizedExpression t decls exprSize
    (es, _) <- genCsvExprs t (n - 1) decls exprSize
    s1 <- genWhiteSpaces
    s2 <- genWhiteSpaces
    return (e ++ s1 ++ ',' : s2 ++ es, t)

-- | genOperator:  ResultType -> Gen (String, LeftArgtype, RightArgType)
genOperator :: BoaType -> Gen (String, BoaType, BoaType)
genOperator TBoolean = do
    o <-
        oneof
            [ return "=="
            , return "!="
            , return "<"
            , return "<="
            , return ">"
            , return ">="
            , return "in"
            , return "not in"
            ]
    case o of
        "in" -> oneof [return (o, TAny, TList), return (o, TText, TText)]
        "not in" -> oneof [return (o, TAny, TList), return (o, TText, TText)]
        "<" -> do
            t <- genType
            return (o, t, t)
        "<=" -> do
            t <- genType
            return (o, t, t)
        ">" -> do
            t <- genType
            return (o, t, t)
        ">=" -> do
            t <- genType
            return (o, t, t)
        "==" -> return (o, TAny, TAny)
        "!=" -> return (o, TAny, TAny)
genOperator TNumber = do
    o <-
        oneof [return "+", return "-", return "*", return "//", return "%"]
    t1 <- frequency [(1, return TBoolean), (2, return TNumber)]
    t2 <- frequency [(5, return TNumber), (1, return TBoolean)]
    return (o, t1, t2)
genOperator TText = do
    o <- oneof [return "+", return "*"]
    case o of
        "+" -> return (o, TText, TText)
        "*" -> do
            l <- oneof [return TNumber, return TBoolean, return TText]
            case l of
                TText -> do
                    r <- oneof [return TNumber, return TBoolean]
                    return (o, TText, r)
                _ -> return (o, l, TText)
genOperator TList = do
    o <- oneof [return "+", return "*"]
    case o of
        "+" -> return (o, TList, TList)
        "*" -> do
            l <- oneof [return TNumber, return TBoolean, return TList]
            case l of
                TList -> do
                    r <- oneof [return TNumber, return TBoolean]
                    return (o, TList, r)
                _ -> return (o, l, TList)
genOperator TAny = do
    t <-
        oneof [return TBoolean, return TNumber, return TText, return TList]
    genOperator t

eqOperator :: Gen String
eqOperator = oneof [return "==", return "!="]

relOperator :: Gen String
relOperator = oneof [return "<", return "<=", return ">", return ">="]

inOperator :: Gen String
inOperator = oneof [return "in", return "not in"]

typeGen :: Gen BoaType
typeGen =
    oneof
        [ return TNone
        , return TBoolean
        , return TNumber
        , return TText
        , return TList
        ]

exprLiteral :: Decls -> Gen (String, BoaType)
exprLiteral decls
    | Map.null decls = oneof [numConst, stringConst, none, true, false]
    | True = oneof [numConst, stringConst, none, true, false, ident]
  where
    numConst = do
        n <- show <$> arbitrarySizedNatural
        return (n, TNumber)
    stringConst = do
        s <- sized sizedStringConst
        return (s, TText)
    none = return ("None", TNone)
    true = return ("True", TBoolean)
    false = return ("False", TBoolean)
    ident = elements $ Map.toList decls

sizedStringConst :: Int -> Gen String
sizedStringConst size = do
    n <- choose (0, size)
    body <-
        concat
            <$> vectorOf
                n
                ( frequency
                    [
                        ( 9
                        , elements $
                            map charToString $
                                filter (`notElem` ['\'', '\\']) $
                                    map chr [32 .. 126] -- printables except ' and \
                        )
                    ,
                        ( 1
                        , oneof
                            [ return "\\'"
                            , return "\\\\"
                            , return "\\n"
                            , return "\\\n"
                            , return "\\\r\n"
                            ]
                        )
                    ]
                )
    return $ '\'' : body ++ "'"
  where
    charToString c = [c]

genEndOfline :: Gen String
genEndOfline =
    oneof
        [return "\r\n", return "\n"]

genWhiteSpace :: Gen String
genWhiteSpace =
    frequency
        [ (1, genEndOfline)
        , (5, return " ")
        ]

genWhiteSpaces :: Gen String
genWhiteSpaces = do
    n <- choose (0, 6)
    genTheSpace n
  where
    genTheSpace :: Int -> Gen String
    genTheSpace 0 = return ""
    genTheSpace n = do
        s <- genWhiteSpace
        case s of
            " " -> do
                rest <- genTheSpace $ n - 1
                return $ concat [s, rest]
            _ -> return s

genWhiteSpaces1 :: Gen String
genWhiteSpaces1 = do
    s <- genWhiteSpaces
    return $ s ++ " "

genSpaces :: Gen String
genSpaces = do
    n <- choose (0, 6)
    s <- vectorOf n (return " ")
    return $ concat s

genSpaces1 :: Gen String
genSpaces1 = do
    s <- genSpaces
    return $ ' ' : s
