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
sizedExpression = undefined

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

{-}
sizedExpression :: Decls -> Int -> Gen Expression
sizedExpression emptyDecls 0 = arbitrary >>= (\x -> return $ Constant x)
sizedExpression decls 0 = oneof [identf, constant]
  where
    identf = elements $ map Variable identifiers
    constant = arbitrary >>= (\x -> return $ Constant x)
sizedExpression identifiers n =
    frequency
        [
            ( 1
            , do
                let i = "var_" ++ (show $ length identifiers)
                e1 <- sizedExpression identifiers $ n `div` 5
                e2 <- sizedExpression (i : identifiers) $ 4 * n `div` 5
                return $ Let i e1 e2
            )
        ,
            ( 1
            , do
                e1 <- sizedExpression identifiers $ n `div` 2
                e2 <- sizedExpression identifiers $ n `div` 2
                o <- arbitrary
                return $ Operator o e1 e2
            )
        ]
-}
