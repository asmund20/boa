module Interpreter where

import Control.Monad
import Data.List (find, intercalate, isInfixOf, unwords)
import Data.Maybe (catMaybes)
import Syntax

type Output = [String]

type ErrorMessage = String
data RuntimeError
    = UnboundVariable VariableName
    | BadFunction FunctionName
    | BadArgument ErrorMessage
    deriving (Eq, Show)

data Range = OneArgR Int | TwoArgR Int Int | ThreeArgR Int Int Int deriving (Eq)

instance Show Range where
    show (OneArgR i) = "range(0, " ++ show i ++ ")"
    show (TwoArgR i1 i2) = "range(" ++ show i1 ++ ", " ++ show i2 ++ ")"
    show (ThreeArgR i1 i2 i3) = "range(" ++ show i1 ++ ", " ++ show i2 ++ ", " ++ show i3 ++ ")"

type Environment = [(VariableName, Value)]
type Runtime a = Environment -> (Either RuntimeError a, Output)

newtype Boa a = Boa {run :: Runtime a}

-- Helper function for bind definition
unwrap :: Boa a -> Runtime a
unwrap (Boa b) = b

-- Monad instance.
instance Monad Boa where
    return = pure -- defined in Applicative instance because the lsp wants it
    -- (>>=) :: Boa a -> (a -> Boa b) -> Boa b

    b1 >>= b2 =
        Boa
            ( \env ->
                let (x1, o1) = run b1 env
                    (x2, o2) = case x1 of
                        Left l -> (Left l, [])
                        Right r -> run (b2 r) env
                 in (x2, o1 <> o2)
            )
instance Functor Boa where
    fmap = liftM
instance Applicative Boa where
    pure x = Boa (const (Right x, []))
    (<*>) = ap

-- Operations of the Boa monad
abort :: RuntimeError -> Boa a
abort re = Boa (const (Left re, []))

look :: VariableName -> Boa Value
look var =
    Boa
        ( \env ->
            ( case find (\(f, _) -> f == var) env of
                Nothing -> Left $ UnboundVariable var
                Just (_, v) -> Right v
            , []
            )
        )

-- type Environment = [(VariableName, Value)]
-- type Runtime a = Environment -> (Either RuntimeError a, Output)
bind :: VariableName -> Value -> (Boa a -> Boa a)
bind name value action = Boa (\env -> run action ((name, value) : env))

output :: String -> Boa ()
output s = Boa (const (Right mempty, [s]))

-- Helper functions for interpreter
truthy :: Value -> Bool
truthy None = False
truthy (Boolean b) = b
truthy (Number 0) = False
truthy (Text []) = False
truthy (List []) = False
truthy _ = True

boolToInt :: Bool -> Integer
boolToInt False = 0
boolToInt True = 1

operate :: OperationSymbol -> Value -> Value -> Either ErrorMessage Value
operate Plus (Number l) (Number r) = Right $ Number $ l + r
operate Plus (Boolean l) (Boolean r) = Right $ Number $ boolToInt l + boolToInt r
operate Plus (Boolean l) (Number r) = Right $ Number $ boolToInt l + r
operate Plus (Number l) (Boolean r) = Right $ Number $ l + boolToInt r
operate Plus (List l) (List r) = Right $ List $ l ++ r
operate Plus (Text l) (Text r) = Right $ Text $ l ++ r
operate Minus (Number l) (Number r) = Right $ Number $ l - r
operate Minus (Boolean l) (Boolean r) = Right $ Number $ boolToInt l - boolToInt r
operate Minus (Boolean l) (Number r) = Right $ Number $ boolToInt l - r
operate Minus (Number l) (Boolean r) = Right $ Number $ l - boolToInt r
operate Times (Number l) (Number r) = Right $ Number $ l * r
operate Times (Boolean l) (Boolean r) = Right $ Number $ boolToInt l * boolToInt r
operate Times (Boolean l) (Number r) = Right $ Number $ boolToInt l * r
operate Times (Number l) (Boolean r) = Right $ Number $ l * boolToInt r
operate Times (Number l) (Text r) = Right $ Text $ take (fromIntegral l * length r) (concat $ repeat r)
operate Times (Text l) (Number r) = Right $ Text $ take (fromIntegral r * length l) (concat $ repeat l)
operate Times (Boolean l) (Text r) =
    Right $ Text $ take (fromIntegral (boolToInt l) * length r) (concat $ repeat r)
operate Times (Text l) (Boolean r) =
    Right $ Text $ take (fromIntegral (boolToInt r) * length l) (concat $ repeat l)
operate Times (Number l) (List r) = Right $ List $ take (fromIntegral l * length r) (concat $ repeat r)
operate Times (List l) (Number r) = Right $ List $ take (fromIntegral r * length l) (concat $ repeat l)
operate Times (Boolean l) (List r) =
    Right $ List $ take (fromIntegral (boolToInt l) * length r) (concat $ repeat r)
operate Times (List l) (Boolean r) =
    Right $ List $ take (fromIntegral (boolToInt r) * length l) (concat $ repeat l)
operate Div (Number l) (Number r) = case r of
    0 -> Left "Division by zero"
    _ -> Right $ Number $ l `div` r
operate Div (Boolean l) (Boolean r) = case r of
    False -> Left "Division by zero"
    True -> Right $ Number $ boolToInt l `div` boolToInt r
operate Div (Boolean l) (Number r) = case r of
    0 -> Left "Division by zero"
    _ -> Right $ Number $ boolToInt l `div` r
operate Div (Number l) (Boolean r) = case r of
    False -> Left "Division by zero"
    _ -> Right $ Number $ l `div` boolToInt r
operate Mod (Number l) (Number r) = case r of
    0 -> Left "Modulo by zero"
    _ -> Right $ Number $ l `mod` r
operate Mod (Boolean l) (Boolean r) = case r of
    False -> Left "Modulo by zero"
    True -> Right $ Number $ boolToInt l `mod` boolToInt r
operate Mod (Boolean l) (Number r) = case r of
    0 -> Left "Modulo by zero"
    _ -> Right $ Number $ boolToInt l `mod` r
operate Mod (Number l) (Boolean r) = case r of
    False -> Left "Modulo by zero"
    _ -> Right $ Number $ l `mod` boolToInt r
operate Eq l r = Right $ Boolean $ equalValues l r
operate Less l r = Boolean <$> lessValue l r
operate Greater l r = Boolean <$> lessValue r l
operate In (Boolean l) (List r) = Right $ Boolean $ (Number $ boolToInt l) `elem` r || (Boolean l) `elem` r
operate In l (List r) = Right $ Boolean $ l `elem` r
operate In (Text l) (Text r) = Right $ Boolean $ l `isInfixOf` r
operate op v1 v2 =
    Left $
        "Operator "
            ++ show op
            ++ " with arguments "
            ++ show v1
            ++ ", "
            ++ show v2
            ++ "."

lessValue :: Value -> Value -> Either ErrorMessage Bool
lessValue None None = Right False
lessValue (Number l) (Number r) = Right $ l < r
lessValue (Boolean l) (Boolean r) = Right $ l < r
lessValue (Boolean l) (Number r) = Right $ (boolToInt l) < r
lessValue (Number l) (Boolean r) = Right $ l < boolToInt r
lessValue (Text l) (Text r) = Right $ l < r
lessValue (List []) (List r) = Right $ not $ null r
lessValue (List l) (List []) = Right False
lessValue (List (l : ls)) (List (r : rs)) = pure (&&) <*> (lessValue l r) <*> lessValue (List ls) (List rs)
lessValue l r =
    Left $
        "Operator < "
            ++ " with arguments "
            ++ show l
            ++ ", "
            ++ show r
            ++ "."

equalValues :: Value -> Value -> Bool
equalValues None None = True
equalValues l@(Number _) r@(Number _) = l == r
equalValues l@(Boolean _) r@(Boolean _) = l == r
equalValues (Boolean l) (Number r) = (boolToInt l) == r
equalValues (Number l) (Boolean r) = l == boolToInt r
equalValues l@(Text _) r@(Text _) = l == r
equalValues l@(List []) r@(List _) = l == r
equalValues l@(List _) r@(List []) = l == r
equalValues (List (l : ls)) (List (r : rs)) = (equalValues l r) && equalValues (List ls) (List rs)
equalValues _ _ = False

prettyValue :: Value -> String
prettyValue = prettyValue' False
  where
    prettyValue' _ None = "None"
    prettyValue' _ (Boolean True) = "True"
    prettyValue' _ (Boolean False) = "False"
    prettyValue' _ (Number n) = show n
    prettyValue' False (Text s) = s
    prettyValue' True (Text s)
        | '"' `elem` s = '\'' : concat (map singles s) ++ "'"
        | '\'' `elem` s = '"' : concat (map doubles s) ++ "\""
        | otherwise = '\'' : concat (map singles s) ++ "'"
      where
        doubles c = case c of
            '\\' -> "\\\\"
            '"' -> "\\\""
            '\n' -> "\\n"
            c -> c : []
        singles c = case c of
            '\\' -> "\\\\"
            '\'' -> "\\'"
            '\n' -> "\\n"
            c -> c : []
    prettyValue' _ (List vs) =
        "[" ++ intercalate ", " (map (prettyValue' True) vs) ++ "]"

apply :: FunctionName -> FunctionArguments -> Boa Value
apply name arguments = case name of
    "range" -> case arguments of
        [stop] -> range (Number 0) stop (Number 1)
        [start, stop] -> range start stop (Number 1)
        [_, _, Number 0] -> abort $ BadArgument "Third argument to range must be nonzero"
        [_, _, Boolean False] -> abort $ BadArgument "Third argument to range must be nonzero"
        [start, stop, step] -> range start stop step
        _ ->
            abort $
                BadArgument $
                    "Bad arguments to range:" ++ (unwords $ map show arguments)
    "print" -> output (unwords $ map prettyValue arguments) >> return None
    _ -> abort $ BadFunction name
  where
    toNumeric :: Value -> Maybe Integer
    toNumeric (Boolean b) = Just $ boolToInt b
    toNumeric (Number n) = Just n
    toNumeric _ = Nothing
    wrongArgument :: Int -> Maybe a -> String
    wrongArgument _ (Just _) = ""
    wrongArgument 1 Nothing = "First range argument is not numeric. "
    wrongArgument 2 Nothing = "Second range argument is not numeric. "
    wrongArgument 3 Nothing = "Third range argument is not numeric. "
    range :: Value -> Value -> Value -> Boa Value
    range startValue stopValue stepValue = do
        let startMaybe = toNumeric startValue
            stopMaybe = toNumeric stopValue
            stepMaybe = toNumeric stepValue
            errorMessage =
                wrongArgument 1 startMaybe
                    ++ wrongArgument 2 stopMaybe
                    ++ wrongArgument 3 stepMaybe
        if not $ null errorMessage
            then abort $ BadArgument errorMessage
            else do
                let Just start = startMaybe
                    Just stop = stopMaybe
                    Just step = stepMaybe
                return $
                    List $
                        takeWhile
                            (\(Number v) -> (step > 0 && v < stop) || (step < 0 && v > stop))
                            ( map
                                (\i -> Number (start + step * i))
                                [0 ..]
                            )

-- do
--     let start'' = toNumeric start'
--         stop'' = toNumeric stop'
--         step'' = toNumeric step'
--         error = concat $ lefts [start'', stop'', step'']
--      in case error of
--             _ : _ -> undefined -- return error message
--             [] -> do
--                 let [start, stop, step] = rights [start'', stop'', step'']

-- Helper function for managing environments in list comprehensions. Takes an environment
-- and an expression and makes a Boa Value monad from it
evalUnderContext :: [(VariableName, Value)] -> Expression -> Boa Value
evalUnderContext context expr = foldl (\acc (n, v) -> bind n v acc) (eval expr) context

-- Main functions of interpreter
eval :: Expression -> Boa Value
eval (Operation op e1 e2) =
    do
        v1 <- eval e1
        v2 <- eval e2
        case operate op v1 v2 of
            (Right value) -> return value
            (Left err) -> abort $ BadArgument err
eval (Variable var) = look var
eval (Constant a) = return a
eval (Not e) = do
    v <- truthy <$> eval e
    return $ Boolean (not v)
eval (Call name input) = do
    args <- eval $ ListExpression input
    case args of
        List a -> apply name a
eval (ListExpression []) = return $ List []
eval (ListExpression (x : xs)) = do
    x <- eval x
    xs <- eval $ ListExpression xs
    case xs of
        List xs -> return $ List (x : xs)
eval (ListComprehension e cs) = do
    finalContexts <- processClauses [[]] cs
    results <- mapM (`evalUnderContext` e) finalContexts
    return $ List results
  where
    processClauses ::
        [[(VariableName, Value)]] -> [Clause] -> Boa [[(VariableName, Value)]]
    processClauses contexts [] = return contexts
    processClauses contexts ((If condExpr) : rest) = do
        keptContexts <-
            catMaybes
                <$> mapM
                    ( \ctx -> do
                        val <- evalUnderContext ctx condExpr
                        if truthy val
                            then return (Just ctx)
                            else return Nothing
                    )
                    contexts
        processClauses keptContexts rest
    processClauses contexts ((For ident iterableExpr : cs)) =
        concat
            <$> mapM
                ( \ctx -> do
                    res <- evalUnderContext ctx iterableExpr
                    case res of
                        List l -> processClauses ([(ident, v) : ctx | v <- l]) cs
                        Text t -> processClauses ([(ident, Text $ v : []) : ctx | v <- t]) cs
                        _ ->
                            abort $
                                BadArgument
                                    "For clause must have either a string or a list to the right of 'in'."
                )
                contexts

exec :: Program -> Boa ()
exec [] = return ()
exec (Define x body : program) =
    do
        v <- eval body
        bind x v (exec program)
exec (Execute thing : program) =
    do
        eval thing
        exec program

execute :: Program -> (Output, Maybe RuntimeError)
execute program = case run (exec program) [] of
    (Right _, output) -> (output, Nothing)
    (Left e, output) -> (output, Just e)
