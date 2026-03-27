module Gen.BoaExpression where

import Data.Char (chr)
import qualified Data.Map as Map
import Interpreter
import Parser
import Syntax
import Test.Tasty.QuickCheck

newtype BoaExpression = E String deriving (Eq)

instance Show BoaExpression where
    show (E p) = p

instance Arbitrary BoaExpression where
    arbitrary = scale (\n -> (n `mod` 4) + 2) $ E <$> addPrint <$> sized sizedExpression

addPrint :: String -> String
addPrint s = "print(" ++ s ++ ")"

sizedExpression :: Int -> Gen String
sizedExpression 0 = frequency [(4, item), (1, list)]
sizedExpression n = do
    t <- sizedExpression 0
    o <- operator
    rest <- sizedExpression (n - 1)
    return $ t ++ o ++ rest

number :: Gen String
number = show <$> elements [-9 .. 9]

string :: Gen String
string = do
    n <- number
    return $ '\'' : n ++ "'"

word :: Gen String
word = elements $ replicate 10 "True" ++ replicate 8 "False" ++ ["None"]

item :: Gen String
item = oneof [number, string, word]

list :: Gen String
list = do
    i <- item
    return $ '[' : i ++ "]"

operator :: Gen String
operator =
    elements
        [ "+"
        , "-"
        , "*"
        , "//"
        , "%"
        ]
