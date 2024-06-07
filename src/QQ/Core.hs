{-# LANGUAGE OverloadedStrings   #-}
module QQ.Core where

import Data.Text(Text)
import qualified Control.Monad.State.Strict as State

{-
>>> evaluate (Val (Con "True"))
Con "True"
-}

type Env = [(Var,Value)]

evaluate :: Exp -> Value
evaluate = evaluateWith []

evaluateWith :: Env -> Exp -> Value
evaluateWith st e =
     case State.runState (eval e) st of
        (v,[]) -> v
        (v,fs) -> error $ "Unexpected free variables: " ++ show fs

type Var = Text

eval :: Exp -> EvalM Value
eval (Val v) = return v

type EvalM = State.State [(Text, Value)]

data Exp = 
    Fun [(Value,Exp)]
    | Val Value
        deriving Show

data Value = 
    Var Text 
    | Con Text     
        deriving Show

-- match (Var v) r = 
