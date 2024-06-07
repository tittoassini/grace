{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TupleSections #-}
module QQ.Core where

import Data.Text(Text)
import qualified Data.Text as T
import qualified Control.Monad.State.Strict as State
import Data.Char (isLower,isUpper)
import Data.Maybe

{-
>>> e $ App (Fun [(PCon "False",conE "True"),(PCon "True",conE "False")]) (conE "False")
Con "True"

>>> e $ var "a"
Free "a" (-1)

>>> e (Val (Con "True"))
Con "True"

-}

e :: Exp -> Value
e = evaluate []

-- evaluate :: Exp -> Value
-- evaluate = evaluateWith []

-- evaluate :: Env -> Exp -> Value
evaluate env e =
     case State.runState (eval e) env of
        (v,[]) -> v
        (v,fs) -> error $ "Unexpected free variables: " ++ show fs

type Env = [(Var,Value)]

type Var = Text

conE = Val . con 

con t   | T.length t >0 &&  isUpper (T.head t) = Con t
        | otherwise = error $ "wrong syntax: " ++ T.unpack t  

var :: Text -> Exp
var t | T.length t >0 &&  isLower (T.head t) = Var t 0
      | otherwise = error $ "wrong syntax: " ++ T.unpack t  

eval :: Exp -> EvalM Value
eval (Var v i) = do
    env <- State.get
    return $ lookupVariable v i env

eval (Val v) = return v

eval (Fun f) = do
    env <- State.get
    return $ Clo $ Closure f env

eval (App f a) = do
    f' <- eval f
    a' <- eval a
    -- env <- State.get
    return $ apply f' a'

apply :: Value -> Value -> Value
apply (Clo (Closure branches env)) a = do
    let matches = filter isJust $ map (\(p,body) -> (,body) <$> match p a) branches   
    case matches of 
        [] -> error "no matching branches"
        (Just (binds,body):_) -> evaluate (env ++ binds) body
        _ -> error "impossible"

apply notF _ = error $ show notF ++ " is not a function" 

{-| A `Closure` captures the current evaluation environment in order to defer
    evaluation until the value of some bound variable is known

    You can think of @Closure name env expression@ as essentially the same thing
    as @\\value -> evaluate ((name, value) : env) e@, except stored using a
    first-order representation.  In fact, you convert to the latter
    representation using `Grace.Normalize.instantiate`.

    This provides efficiency comparable to a higher-order abstract syntax
    tree, except using a first-order representation.
-}
data Closure = Closure [Branch] Env deriving (Show)

type Branch = (Pattern,Exp)

type EvalM = State.State [(Text, Value)]

data Exp = 
    App Exp Exp
    | Fun [Branch]
    | Var Text Int
    | Val Value
        deriving Show

data Value =
      Free Text Int      
    | Con Text
    | Clo Closure
        deriving Show

data Pattern = PVar Text | PCon Text deriving Show

match :: Pattern -> Value -> Maybe [(Text,Value)]
match (PVar v) r = Just [(v,r)]
match (PCon c) (Con c') | c == c' = Just []
match _ _ = Nothing

{-| Lookup a variable from an ordered environment of name-value pairs using the
    variable's name and index
-}
lookupVariable
    :: Text
    -- ^ Variable name
    -> Int
    -- ^ Variable index
    -> [(Text, Value)]
    -- ^ Evaluation environment
    -> Value
lookupVariable name index environment =
    case environment of
        (key, value) : rest ->
            if name == key
            then if index == 0
                 then value
                 else lookupVariable name (index - 1) rest
            else lookupVariable name index rest
        [] ->
            -- In the `Value` type, free variables are stored using negative
            -- indices (starting at -1) to avoid collision with bound variables
            --
            -- >>> evaluate [] "x"
            -- Variable "x" (-1)
            --
            -- This has the nice property that `quote` does the right thing when
            -- converting back to the `Syntax` type.
            Free name (negate index - 1)
