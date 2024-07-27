{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module QQ.Core where

import Control.Monad ((>=>))
import qualified Control.Monad.State.Strict as State
import Data.Char (isLower, isUpper)
import Data.Maybe
import Data.String ()
import Data.Text (Text)
import qualified Data.Text as T
import Grace.Test (v)
import Language.Haskell.TH (valD, valueAnnotation)
import Network.HTTP.Types (gone410)
import Text.ParserCombinators.ReadP (look)
import qualified ZM.Parser as P

{-
Con "True"

>>> e $ var "a"
Variable not in scope: e :: Exp -> b_a14md[sk:1]

>>> e (Val (Con "True"))
Variable not in scope: e :: Exp -> t_a14oE[sk:1]

>>> P.parseMaybe P.value "Cons {head:a, tail:List a}"
No instance for (Show1 (ValF Literal Void))
  arising from a use of `evalPrint'
In a stmt of an interactive GHCi command: evalPrint it_a14py

-}
-- n :: Maybe P.Value
-- n = P.parseMaybe P.value "Nil" -- Cons {head:a, tail:List a}"

-- e :: Exp -> Value
-- e = evaluate []

-- evaluate :: Exp -> Value
-- evaluate = evaluateWith []

evaluate :: Env -> Exp -> Value
evaluate env e =
    case State.runState (eval e) env of
        (v, []) -> v
        (v, fs) -> error $ "Unexpected free variables: " ++ show fs

type Env = [(Var, Value)]

type Var = Text

conE :: Text -> Exp
conE = Val . con

con :: Text -> Value
con t
    | T.length t > 0 && isUpper (T.head t) = Con t
    | otherwise = error $ "wrong syntax: " ++ T.unpack t

var :: Text -> Exp
var t
    | T.length t > 0 && isLower (T.head t) = Var t 0
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
    let matches = filter isJust $ map (\(p, body) -> (,body) <$> match p a) branches
    case matches of
        [] -> error "no matching branches"
        (Just (binds, body) : _) -> evaluate (env ++ binds) body
        _ -> error "impossible"
apply notF _ = error $ show notF ++ " is not a function"

{- | A `Closure` captures the current evaluation environment in order to defer
    evaluation until the value of some bound variable is known

    You can think of @Closure name env expression@ as essentially the same thing
    as @\\value -> evaluate ((name, value) : env) e@, except stored using a
    first-order representation.  In fact, you convert to the latter
    representation using `Grace.Normalize.instantiate`.

    This provides efficiency comparable to a higher-order abstract syntax
    tree, except using a first-order representation.
-}
type EvalM = State.State [(Text, Value)]

-- Extension Calculus

-- type Env = [(Text,Val)]

{-
>>> l0 = TApply (TApply (TCons "Cons") (TCons "False")) (TCons "Nil")

>>> evalTerm l0 []
VApply (VApply (Constr "Cons") (Constr "False")) (Constr "Nil")

>>> evalTerm TWildcard []
Wildcard

>>> evalTerm (TVar $ Variable "v" 0) [("v",Constr "True")]
Constr "True"

>>> evalTerm (TVar $ Variable "v" 0) [("v",Constr "True")]
Constr "True"

>>> evalTerm (TVar $ Variable "v" 0) []
undefined variable: Variable "v" 0
-}

evalTerm :: Term -> [(Text, Val)] -> Val
evalTerm term env =
    case term of
        TVar v -> evalVar v env
        TWildcard -> Wildcard
        TCons v -> Constr v
        -- TLit l -> VLit l
        TApply f a -> evalAp f a env
        _ -> error $ "cannot evaluate: " ++ show term

evalVar :: Variable -> [(Text, Val)] -> Val
evalVar var@(Variable v n) env =
    case lookupVar v n env of
        Just val -> val
        Nothing -> error $ "undefined variable: " ++ show var

evalAp :: Term -> Term -> [(Text, Val)] -> Val
evalAp f a env =
    case evalAp_opt (evalTerm f env) (evalTerm a env) of
        Just z -> z
        Nothing -> error $ "produces a match failure: " ++ show (TApply f a)

evalAp_opt :: Val -> Val -> Maybe Val
evalAp_opt f a =
    case f of
        -- VLam
        VCase p env s -> case pmatch p a of
            Just matches -> Just $ evalTerm s (matches ++ env)
            Nothing -> Nothing
        _ -> Just $ VApply f a

-- o -> error $ "evalAp unhandled: " ++ show o

pmatch :: Val -> Val -> Maybe [(Text, Val)]
pmatch p s =
    case (p, s) of
        (Constr c, Constr c') -> check $ c == c'
        (VVar (Variable v 0), s) -> Just [(v, s)] -- BUG?
        (Wildcard, _) -> Just []
        (VApply f1 a1, VApply f2 a2) -> pmatch f1 f2 `unionMatch` pmatch a1 a2
        _ -> Nothing
  where
    check True = Just []
    check False = Nothing

    unionMatch :: Maybe [a] -> Maybe [a] -> Maybe [a]
    unionMatch m1 m2 = (++) <$> m1 <*> m2

-- patval
patval p env =
    case p of
        TVar v -> VVar v -- CHK: Tvar (x, (* n *) _) -> Vvar x
        TWildcard -> Wildcard
        TCons v -> Constr v
        -- TLit v -> Lit v
        TApply f a -> VApply (patval f env) (patval a env)
        _ -> error $ "Not a static pattern: " ++ show p

data Term
    = TVar Variable
    | TWildcard
    | TCons Text -- Variable
    | -- | TLit Lit
      -- | Operand Text [Term]
      TApply Term Term
    | -- | Lam Variable Term
      TCase [Variable] Term Term -- {..} p -> s
    | TChoice Term Term
    | TLetRec
    deriving (Show)

-- instance IsString Term where fromString c = TCons (T.pack c)

-- An unambigous reference to a variable in context
data Variable = Variable Text Int deriving (Eq, Show)

data Val
    = VVar Variable
    | -- | VLit Lit
      Wildcard
    | Constr Text -- Variable
    | VApply Val Val
    | VCase Val [(Text, Val)] Term
    | VChoice Value Value
    deriving
        ( -- | Ext [Text] Val Term Term
          Show
        )

data Lit = LitInt Int | LitText Text deriving (Show)

-- data DataVal =
--     DVar Variable
--     | Cons Text
--     | Wildcard
--     | Compound DataVal Val
--         deriving Show
-- data Val =
--     DVal DataVal
--     | Ext [Text] Val Term Term
--         deriving Show

data V
    = VData Data
    | DClosure Closure

data Data
    = DCon Text
    | DApp Data Term
    deriving (Show)

-- Basic Calculus
data Exp
    = App Exp Exp
    | Fun [Branch]
    | Var Text Int
    | Val Value
    deriving (Show)

type Branch = (Pattern, Exp)

-- What cannot be further evaluated
data Value
    = Free Variable -- ?
    | Con Text
    | VApp Value Value -- Data
    | Clo Closure
    deriving (Show)

-- Atoms
data Literal

data Closure = Closure [Branch] Env deriving (Show)

data Pattern = PVar Text | PCon Text deriving (Show)

match :: Pattern -> Value -> Maybe [(Text, Value)]
match (PVar v) r = Just [(v, r)]
match (PCon c) (Con c') | c == c' = Just []
match _ _ = Nothing

{- | Lookup a variable from an ordered environment of name-value pairs using the
    variable's name and index
-}
lookupVariable ::
    -- | Variable name
    Text ->
    -- | Variable index
    Int ->
    -- | Evaluation environment
    [(Text, Value)] ->
    Value
lookupVariable name index environment =
    case environment of
        (key, value) : rest ->
            if name == key
                then
                    if index == 0
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
            Free $ Variable name (negate index - 1)

lookupVar :: Text -> Int -> [(Text, a)] -> Maybe a
lookupVar name index environment =
    case environment of
        (key, value) : rest ->
            if name == key
                then
                    if index == 0
                        then Just value
                        else lookupVar name (index - 1) rest
                else lookupVar name index rest
        [] -> Nothing
