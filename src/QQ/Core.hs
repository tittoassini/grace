{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TupleSections #-}
module QQ.Core where

import Data.Text(Text)
import qualified Data.Text as T
import qualified Control.Monad.State.Strict as State
import Data.Char (isLower,isUpper)
import Data.Maybe
import Network.HTTP.Types (gone410)
import Grace.Test (v)
import Control.Monad ((>=>))
import Text.ParserCombinators.ReadP (look)
import Language.Haskell.TH (valD, valueAnnotation)

{-
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

evaluate :: Env -> Exp -> Value
evaluate env e =
     case State.runState (eval e) env of
        (v,[]) -> v
        (v,fs) -> error $ "Unexpected free variables: " ++ show fs

type Env = [(Var,Value)]

type Var = Text

conE :: Text -> Exp
conE = Val . con 

con :: Text -> Value
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
type EvalM = State.State [(Text, Value)]

-- Dynamic Pattern Calculus
-- data Term = 
--     BVar Text
--     | MVar Text
--     | TApp Term Term
--     | Case [Text] Term Term

-- Extension Calculus

-- type Env = [(Text,Val)]

evalTerm :: Term -> [(Text, Val)] -> Val
evalTerm term env =
    case term of
        TVar v -> evalVar v env
        TWildcard -> Wildcard
        TCons v -> Constr v
        -- TLit l -> VLit l
        TApply f a -> evalAp f a env
        _ -> error $ "cannot evaluate: " ++ show term

evalAp f a env = 
    case evalAp_opt (evalTerm f env) (evalTerm a env) of
        Just z -> z
        Nothing -> error $ "produces a match failure: " ++ show (TApply f a)

evalAp_opt f a = 
    case f of 
        -- VLam 
        VCase p env s -> case pmatch p a of
            Just matches -> Just $ evalTerm s (matches ++ env) 
            Nothing -> Nothing  

pmatch :: Val -> Val -> Maybe [(Text,Val)]
pmatch p s = 
    case (p,s) of
        (Constr c ,Constr c') -> check $ c == c'
        -- (Var v,s) -> Just [(v,s)] 
        -- (WildCard,_) -> Just []
        (VApply f1 a1,VApply f2 a2) -> pmatch f1 f2 `unionMatch` pmatch a1 a2
        _ -> Nothing

    where
        check True = Just []
        check False = Nothing

        unionMatch :: Maybe [a] -> Maybe [a] -> Maybe [a]
        unionMatch m1 m2 = (++) <$> m1 <*> m2
        -- Nothing `unionMatch` _ = Nothing
        -- _ `unionMatch` Nothing = Nothing
        -- Just e1 `unionMatch` Just e2 = Just $ e1 ++ e2

-- pmatch (VApp f1 a1) _ = return False


-- patval
patval p env = 
    case p of 
        TVar v -> VVar v -- CHK: Tvar (x, (* n *) _) -> Vvar x
        TWildcard -> Wildcard
        TCons v -> Constr v
        -- TLit v -> Lit v
        TApply f a -> VApply (patval f env) (patval a env) 
        _ -> error $ "Not a static pattern: " ++ show p  

evalVar :: Variable -> [(Text, Val)] -> Val
evalVar (Variable v n) env = 
    case lookupVar v n env of 
        Just val -> val
        Nothing -> error "undefined variable"
data Term =
    TVar Variable
    | TWildcard
    | TCons Variable
    -- | TLit Lit
    -- | Operand Text [Term]
    | TApply Term Term
    -- | Lam Variable Term
    | TCase [Variable] Term Term -- {..} p -> s
    | TChoice Term Term
    | TLetRec 
        deriving Show



data Variable = Variable Text Int deriving (Eq,Show)

-- evalCase (Case theta@[] p s) env = 

-- data Case = Case [Variable] Term Term -- {..} p -> s
--     deriving Show

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

data Val = 
    VVar Variable
    -- | VLit Lit
    | Wildcard
    | Constr Variable
    | VApply Val Val
    | VCase Val [(Text, Val)] Term
    | VChoice Value Value
    -- | Ext [Text] Val Term Term 
        deriving Show

data Lit = LitInt Int | LitText Text deriving Show

data Exp = 
    App Exp Exp
    | Fun [Branch]
    | Var Text Int
    | Val Value
        deriving Show

type Branch = (Pattern,Exp)

-- What cannot be further evaluated
data Value =
      Free Variable
    | Con Text
    | VApp Value Value
    | Clo Closure
        deriving Show

-- Atoms
data Literal 

data Closure = Closure [Branch] Env deriving (Show)

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
            Free $ Variable name (negate index - 1)


lookupVar :: Text -> Int -> [(Text, Val)] -> Maybe Val
lookupVar name index environment =
    case environment of
        (key, value) : rest ->
            if name == key
            then if index == 0
                 then Just value
                 else lookupVar name (index - 1) rest
            else lookupVar name index rest
        [] -> Nothing