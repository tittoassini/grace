{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module QQ.Eval (
    -- * Normalization
    eval,
    -- , quote
    apply,
) where

import Data.Bifunctor (Bifunctor (..), first)
import Data.Maybe
import Data.Text (Text, unpack)
import qualified Data.Text as T
import ZM.Parser hiding (Value, value)
import ZM.Parser.Exp (loadMdl)

{- eval an expression, leaving behind a `Value` free of reducible
    sub-expressions

    This function uses separate types for the input (i.e. `Syntax`) and the
    output (i.e. `Value`) in order to avoid wastefully evaluating the same
    sub-expression multiple times.

>>> let e src = show . eval [] <$> parseMdl src

let pe src = show . pretty . eval [] <$> parseMdl src

>>> e "11"

>>> e "Cons 1 Nil"
Right "VApp (VApp (VCon \"Cons\") (VLit (LInteger 1))) (VCon \"Nil\")"

>>> parseMdl "{& x=1 \n&}"
Right (Ann 0 (Arr (Bracket {open = '{', close = '}', op = Just "&", values = [Ann 4 (App (Ann 4 (App (Ann 4 (Infix "=")) (Ann 3 (Prefix "x")))) (Ann 5 (Lit (LInteger 1))))]})))

>>> e "{T->F\nF->T}F"
Right "VCon \"T\""

OHI!
>>> e "{x->x}T"
Right "VCon \"T\""

>>> e "{Cons h h -> h}(Cons 1 Nil)"
Right "VLit (LInteger 1)"

>>> e "1 2"
Right "VApp (VLit (LInteger 1)) (VLit (LInteger 2))"

>>> e "{&\nx=1\ny=2\nf={T->x\nF->y\n}\nf1=f T\nf2=f F\n&} \"x\""

>>> e "{&\nl=len Nil\nlen = {\nNil -> Z\nCons h t -> S (len t)}\n&} \"l\""

>>> e "{&\nl=len (Cons 1 (Cons 2 Nil))\nlen = {\nNil -> Z\nCons h t -> S (len t)}\n&} \"l\""

Recursive definitions

{
l1 = len (Cons 1 (Cons 2 Nil))

len = {
    Nil -> 0
    Cons h t -> 1 + len t
    }

l2 = len (Cons 1 (Cons 2 Nil))
}

l1 = len (1 : 2 : Nil)

fix f = f '(fix f)
fact = \fact n -> if n == 0 then 1 else fact (n - 1) * n

Infix Constructors

char parser

char = {
    Nil -> None
    h:t -> Some (t,h)
}

ifte F _ y = y
ifte T x _ = x
ifthenelse = {
    F -> {_ -> {y -> y)}
    T -> {x -> {_ -> x}}
}

-}
e src = show . eval [] <$> parseMdl src

t f = loadMdl $ concat ["qq-src/", f, ".qq"]

eval ::
    -- | Evaluation environment (starting at @[]@ for a top-level expression)
    Env ->
    -- | Surface syntax
    Exp ->
    -- | Result, free of reducible sub-expressions
    Value
eval env (Ann _ (App f a)) =
    apply f' a'
  where
    f' = eval env f
    a' = eval env a
eval env (Ann _ (InfixApp a op b)) =
    apply (apply op' a') b'
  where
    op' = solveVar op 0 env
    a' = eval env a
    b' = eval env b

-- eval env (Ann _ (Infix name)) = solveVar name 0 env

eval _ (Ann _ (Con name)) = VCon name
eval env (Ann _ (Prefix name)) =
    case lookupVar name 0 env of
        Just v -> v
        Nothing -> VMatch name
-- Lists
eval env (Ann _ (Arr (Bracket{open = '[', close = ']', op = Nothing, values = vs}))) =
    foldr (\v l -> VApp (VApp (VCon "::") (eval env v)) l) (VCon "[]") vs
-- Block of (mutually) recursive definitions/Module/Record
eval env (Ann _ (Arr (Bracket{open = '{', close = '}', op = Just "&", values = branches}))) =
    VEnv $ env'' ++ env
  where
    env'' = map (second (eval env')) bs
    env' = map (second (VLazy env'')) bs ++ env
    bs = map evalBranch branches

    evalBranch (Ann _ (InfixApp (Ann _ (Prefix n)) "=" e)) = (n, e)
    -- evalBranch (Ann _ (App (Ann _ (App (Ann _ (Infix "=")) p@((Ann _ (Prefix n))))) e)) = (n, e)
    evalBranch notABranch = err ["not a definition", show notABranch]

-- Lambda/Case
eval env (Ann _ (Arr (Bracket{open = '{', close = '}', op = Nothing, values = branches}))) =
    VLam $ Closure (map evalBranch branches) env
  where
    evalBranch (Ann _ (InfixApp p "->" r)) = (evalPattern p, r)
    -- evalBranch (Ann _ (App (Ann _ (App (Ann _ (Infix "->")) p)) r)) = (evalPattern p, r)
    evalBranch notABranch = err ["not a pattern matching clause:", show notABranch]
-- Generic Bracket evaluation, apply op to each branch
-- TODO: no evaluation
-- as is equivalent to a fold why not writing it as such?
eval env (Ann _ (Arr (Bracket{open = '{', close = '}', op = Just oper, values = vs}))) =
    foldr (\v e -> VApp (VApp (VCon oper) (eval env v)) e) (error "empty seq") vs
eval _ (Ann _ (Arr b@Bracket{})) = err ["unsupported bracket:", show b]
eval _ (Ann _ (Lit l)) = VLit l

evalPattern :: Exp -> Value
evalPattern = eval []

-- apply :: Value -> Value -> Value
apply (VLam c@(Closure branches env)) a = do
    let matches = mapMaybe (\(p, body) -> (,body) <$> match p a) branches
    case matches of
        [] -> err ["no matching patterns in closure:", show c, "applied to", show a]
        ((binds, body) : _) -> eval (binds ++ env) body
apply (VEnv env) (VLit (LString fld)) = solveVar fld 0 env
apply (VLazy env f) a = apply (eval env f) a
-- apply notf _ = err ["not a lambda or a constructor:", show notf]

-- NOTE: accept/suspend all other applications
apply a b = VApp a b

err = error . unwords

-- | Substitute an expression into a `Closure`

-- instantiate :: Closure -> Value -> Value
-- instantiate (Closure name env syntax) value =
--     evaluate ((name, value) : env) syntax

match :: Value -> Value -> Maybe [(Text, Value)]
match p s =
    case (p, s) of
        (VCon c, VCon c') | c == c' -> Just []
        (VMatch v, o) -> Just [(v, o)] -- BUG?
        (VWild, _) -> Just []
        (VApp f1 a1, VApp f2 a2) -> match f1 f2 `unionMatch` match a1 a2
        (VLit l, VLit l') | l == l' -> Just []
        _ -> Nothing
  where
    -- check True = Just []
    -- check False = Nothing

    unionMatch :: Maybe [a] -> Maybe [a] -> Maybe [a]
    unionMatch m1 m2 = (++) <$> m1 <*> m2

type Env = [(Text, Value)]

data Value
    = VApp Value Value
    | VEnv Env -- A list of mutually recursive definitions
    | VLazy Env Exp -- = (\() a) ()
    | VLam Closure
    | VMatch Text -- Matching symbol, e.g. "x"
    | VWild -- Matching wildcard, e.g. _ _aWildcard
    | VCon Text
    | VLit Literal
    deriving
        ( Show
        )

data Closure = Closure [(Value, Exp)] Env deriving (Show)

-- An unambigous reference to a variable in context
data Variable = Variable Text Int deriving (Eq, Show)

solveVar name index environment = fromMaybe (error $ "unknown symbol " ++ unpack name) $ lookupVar name index environment

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
