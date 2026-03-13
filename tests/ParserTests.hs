module ParserTests where

import qualified Data.Set as Set
import Interpreter
import Parser
import Syntax
import Test.Tasty.QuickCheck

data BoaProgram = P String deriving (Show, Eq)

type Decls = Set.Set String

emptyDecls :: Decls
emptyDecls = Set.empty

instance Arbitrary BoaProgram where
    arbitrary = do
        (s, _) <- sized $ sizedProgram emptyDecls
        return $ P s

sizedProgram :: Decls -> Int -> Gen (String, Decls)
sizedProgram decls 0 = oneof [assignment, expression]
  where
    assignment :: Gen (String, Decls)
    assignment = do
        i <- identGen
        s1 <- genSpaces
        s2 <- genSpaces
        (e, _) <- expression
        return (i ++ s1 ++ '=' : s2 ++ e, Set.insert i decls)
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
sizedExpression decls n = undefined

exprLiteral :: Decls -> Gen String
exprLiteral decls = oneof [numConst, sized stringConst, none, true, false, ident]
  where
    numConst = show <$> arbitrarySizedNatural
    stringConst size = do
        n <- choose (0, size)
        vectorOf
            n
            -- TODO: finish the stringConst generator
            ( frequency
                [ (1, elements $ ['a' .. 'z'] ++ ['A' .. 'Z'])
                ]
            )
    none = return "None"
    true = return "True"
    false = return "True"
    ident = elements $ Set.toList decls

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
    return $ foldl (++) [] s
