module Interpreter where

import Control.Monad
import Data.List (find, intercalate, unwords)
import Data.Maybe (catMaybes)
import Syntax

type Output = [String]

type ErrorMessage = String
data RuntimeError
    = UnboundVariable VariableName
    | BadFunction FunctionName
    | BadArgument ErrorMessage
    deriving (Eq, Show)

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
bind name value action = Boa (\env -> let env' = (name, value) : env in run action env')

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
operate Eq (Number l) (Number r) = Right $ Boolean $ l == r
operate Eq (Boolean l) (Number r) = Right $ Boolean $ boolToInt l == r
operate Eq (Number l) (Boolean r) = Right $ Boolean $ l == boolToInt r
operate Eq (Boolean l) (Boolean r) = Right $ Boolean $ l == r
operate Eq (Text l) (Text r) = Right $ Boolean $ l == r
operate Eq (List l) (List r) = Right $ Boolean $ l == r
operate Eq None None = Right $ Boolean True
operate Eq _ _ = Right $ Boolean False
operate Less (Boolean l) (Boolean r) = Right $ Boolean $ l < r
operate Less (Number l) (Number r) = Right $ Boolean $ l < r
operate Less (Text l) (Text r) = Right $ Boolean $ l < r
operate Less (List l) (List r) = Right $ Boolean $ length l < length r
operate Greater (Boolean l) (Boolean r) = Right $ Boolean $ l > r
operate Greater (Number l) (Number r) = Right $ Boolean $ l > r
operate Greater (Text l) (Text r) = Right $ Boolean $ l > r
operate Greater (List l) (List r) = Right $ Boolean $ length l > length r
operate In (List l) (List ((List r) : rs)) = Right $ Boolean $ elem (List l) (List r : rs)
operate In (Number l) (List ((Number r) : rs)) = Right $ Boolean $ elem (Number l) (Number r : rs)
operate In (Text l) (List ((Text r) : rs)) = Right $ Boolean $ elem (Text l) (Text r : rs)
operate In (Boolean l) (List ((Boolean r) : rs)) = Right $ Boolean $ elem (Boolean l) (Boolean r : rs)
operate In _ (List []) = Right $ Boolean False
operate op v1 v2 =
    Left $
        "Operator "
            ++ show op
            ++ " with arguments "
            ++ show v1
            ++ ", "
            ++ show v2
            ++ "."

prettyValue :: Value -> String
prettyValue None = "None"
prettyValue (Boolean True) = "True"
prettyValue (Boolean False) = "False"
prettyValue (Number n) = show n
prettyValue (Text s) = s
prettyValue (List vs) =
    "[" ++ intercalate ", " (map prettyValue vs) ++ "]"

apply :: FunctionName -> FunctionArguments -> Boa Value
apply name arguments = case name of
    "range" -> case arguments of
        [Number x] -> return $ List [Number a | a <- [0 .. x - 1]]
        [Number x, Number y] -> return $ List [Number a | a <- [x .. y - 1]]
        [Number x, Number y, Number z] -> return $ List [Number a | a <- [x, x + z .. y - 1]]
        _ -> abort $ BadArgument $ unwords $ map show arguments
    "print" -> output (unwords $ map prettyValue arguments) >> return None
    _ -> abort $ BadFunction name

-- Helper function for managing environments in list comprehensions
evalUnderContext :: [(VariableName, Value)] -> Expression -> Boa Value
evalUnderContext context expr = foldr (\(n, v) acc -> bind n v acc) (eval expr) context

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
    let initialContexts = [[]]

    let processClauses context [] = return context
        processClauses contexts (cl : rest) = case cl of
            If condExpr -> do
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
            For var iterableExpr -> do
                expandedContextsList <-
                    mapM
                        ( \ctx -> do
                            val <- evalUnderContext ctx iterableExpr
                            case val of
                                List elems -> return [(var, e) : ctx | e <- elems]
                                other -> abort $ BadArgument $ "for needs a list, got " ++ show other
                        )
                        contexts
                let newContexts = concat expandedContextsList
                processClauses newContexts rest
    finalContexts <- processClauses initialContexts cs

    results <- mapM (`evalUnderContext` e) finalContexts

    return $ List results

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
