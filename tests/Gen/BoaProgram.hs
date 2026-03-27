module Gen.BoaProgram where

import Data.Char (chr)
import qualified Data.Map as Map
import Interpreter
import Parser
import Syntax
import Test.Tasty.QuickCheck

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
    arbitrary = resize 13 $ do
        (s, _) <- sized $ sizedProgram emptyDecls
        return $ P s

genOrdType :: Gen BoaType
genOrdType = elements [TBoolean, TNumber, TText]

genType :: Gen BoaType
genType =
    elements
        [ TNone
        , TBoolean
        , TNumber
        , TText
        , TList
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
        (e, _) <- sized $ typedSizedExpression TAny decls
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
    let ident = c : cs
    if ident
        `elem` [ "False"
               , "def"
               , "if"
               , "raise"
               , "None"
               , "del"
               , "import"
               , "return"
               , "True"
               , "elif"
               , "in"
               , "try"
               , "and"
               , "else"
               , "is"
               , "while"
               , "as"
               , "except"
               , "lambda"
               , "with"
               , "assert"
               , "finally"
               , "nonlocal"
               , "yield"
               , "break"
               , "for"
               , "not"
               , "class"
               , "form"
               , "or"
               , "continue"
               , "global"
               , "pass"
               ]
        then
            identGen
        else return ident
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

identifierOrOther ::
    BoaType -> Decls -> Gen (String, BoaType) -> Gen (String, BoaType)
identifierOrOther t d gen = do
    let i = declsOfType t d
    case i of
        [] -> gen
        otherwise ->
            oneof
                [ (\ident -> (ident, t)) <$> elements i
                , gen
                ]

-- data BoaType = TNone | TBoolean | TNumber | TText | TList | TAny
typedSizedExpression ::
    BoaType -> Decls -> Int -> Gen (String, BoaType)
typedSizedExpression TNone decls 0 = identifierOrOther TNone decls (return ("None", TNone))
typedSizedExpression TBoolean decls 0 =
    identifierOrOther
        TBoolean
        decls
        ( elements
            [ ("True", TBoolean)
            , ("False", TBoolean)
            ]
        )
typedSizedExpression TNumber decls 0 =
    identifierOrOther
        TNumber
        decls
        ( do
            n <- show <$> arbitrarySizedIntegral
            return $ (n, TNumber)
        )
typedSizedExpression TText decls 0 =
    identifierOrOther
        TText
        decls
        ( do
            s <- sized sizedStringConst
            return $ (s, TText)
        )
typedSizedExpression TList decls 0 = identifierOrOther TList decls (return ("[]", TAny))
typedSizedExpression TAny decls n = do
    t <- genType
    typedSizedExpression t decls n
typedSizedExpression TNone decls n =
    identifierOrOther
        TNone
        decls
        ( frequency
            [ (1, return ("None", TNone))
            , (2, printCall)
            ]
        )
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
typedSizedExpression TList decls n =
    identifierOrOther
        TList
        decls
        (oneof [listExp, listComprehension, rangeCall])
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
        (l, t) <-
            oneof
                [ typedSizedExpression TList d expLength
                , typedSizedExpression TText d expLength
                ]
        s1 <- genWhiteSpaces
        s2 <- genWhiteSpaces
        s3 <- genWhiteSpaces
        return (" for " ++ s1 ++ i ++ s2 ++ " in " ++ s3 ++ l, Map.insert i t d)
    genIfClause :: Int -> Decls -> Gen (String, Decls)
    genIfClause expLength decls = do
        (e, _) <-
            oneof
                [ typedSizedExpression TAny decls expLength
                , typedSizedExpression TBoolean decls expLength
                ]
        s1 <- genWhiteSpaces
        return $ (" if " ++ s1 ++ e, decls)
    genClauses :: Int -> Int -> Decls -> Gen (String, Decls)
    genClauses 0 expLength decls = return ("", decls)
    genClauses clauseLength expLength decls = do
        (c, decls) <-
            frequency [(1, genForClause expLength decls), (2, genIfClause expLength decls)]
        (cs, decls) <- genClauses (clauseLength - 1) expLength decls
        s1 <- genWhiteSpaces1
        return (c ++ s1 ++ cs, decls)
typedSizedExpression t decls n =
    identifierOrOther
        t
        decls
        ( oneof
            [ binaryExp t decls n
            , parenExp t decls n
            , typedSizedExpression t decls 0
            ]
        )

binaryExp :: BoaType -> Decls -> Int -> Gen (String, BoaType)
binaryExp t decls n = do
    (o, tl, tr) <- genOperator t
    (el, _) <- typedSizedExpression tl decls (n `div` 2)
    (er, _) <- typedSizedExpression tr decls (n `div` 2)
    s1 <- genSpaces
    s2 <- genSpaces
    return ('(' : el ++ s1 ++ o ++ s2 ++ er ++ ")", t)
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
        elements
            [ "=="
            , "!="
            , "<"
            , "<="
            , ">"
            , ">="
            , " in "
            , " not in "
            , "+"
            , "-"
            ]
    case o of
        " in " -> elements [(o, TAny, TList), (o, TText, TText)]
        " not in " -> elements [(o, TAny, TList), (o, TText, TText)]
        "<" -> do
            t <- genOrdType
            return (o, t, t)
        "<=" -> do
            t <- genOrdType
            return (o, t, t)
        ">" -> do
            t <- genOrdType
            return (o, t, t)
        ">=" -> do
            t <- genOrdType
            return (o, t, t)
        "==" -> return (o, TAny, TAny)
        "!=" -> return (o, TAny, TAny)
        "+" -> return (o, TNumber, TNumber)
        "-" -> return (o, TNumber, TNumber)
genOperator TNumber = do
    o <-
        elements ["+", "-", "*", "//", "%"]
    t1 <- frequency [(1, return TBoolean), (2, return TNumber)]
    t2 <- frequency [(5, return TNumber), (1, return TBoolean)]
    return (o, t1, t2)
genOperator TText = do
    o <- elements ["+", "*"]
    case o of
        "+" -> return (o, TText, TText)
        "*" -> do
            l <- elements [TNumber, TBoolean, TText]
            case l of
                TText -> do
                    r <- elements [TNumber, TBoolean]
                    return (o, TText, r)
                _ -> return (o, l, TText)
genOperator TList = do
    o <- elements ["+", "*"]
    case o of
        "+" -> return (o, TList, TList)
        "*" -> do
            l <- elements [TNumber, TBoolean, TList]
            case l of
                TList -> do
                    r <- elements [TNumber, TBoolean]
                    return (o, TList, r)
                _ -> return (o, l, TList)
genOperator TAny = do
    t <-
        elements [TBoolean, TNumber, TText, TList]
    genOperator t

typeGen :: Gen BoaType
typeGen =
    elements
        [ TNone
        , TBoolean
        , TNumber
        , TText
        , TList
        ]

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
                        , elements
                            [ "\\'"
                            , "\\\\"
                            , "\\n"
                            , "\\\n"
                            , "\\\r\n"
                            ]
                        )
                    ]
                )
    return $ '\'' : body ++ "'"
  where
    charToString c = [c]

genEndOfline :: Gen String
genEndOfline = elements ["\r\n", "\n"]

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
