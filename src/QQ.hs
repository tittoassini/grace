{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module QQ where

import Control.Monad
import Data.Text (Text, intercalate, pack)

import qualified Data.Text as T
import Control.Lens (itakingWhile)

newtype State s r = State {runState :: s -> (s,r)}

instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    fmap f (State g) = State $ \s -> let (s',r) = g s in (s', f r)  

instance Applicative (State s) where
    pure :: a -> State s a
    pure x = State $ \s -> (s,x)

    (<*>) :: State s (a -> b) -> State s a -> State s b
    (State f) <*> (State g) = State $ \s -> let (s',f') = f s; (s'',g') = g s' in (s'', f' g')

instance Monad (State s) where
    (>>=) :: State s a -> (a -> State s b) -> State s b
    (State g) >>= f = State $ \s -> let (s',r) = g s in runState (f r) s'

get :: State s s
get = State $ \s -> (s,s)

put :: s -> State s ()
put s = State $ \_ -> (s,())

modify :: (s -> s) -> State s ()
modify f = State $ \s -> (f s,())

class Stateful st s where
    getSt :: st s
    putSt :: s -> st ()
    modifySt :: (s -> s) -> st ()

{- Suspended/stuck constant

A constant whose value has not been assigned yet.

As soon as the value is assigned, all values depending on it will be updated.

Like STM in Haskell where a transaction is suspended until a value is assigned.

-}

-- instance Stateful (State s) where
--     getSt :: State s s
--     getSt = get
--     putSt = put
--     modifySt = modify

-- testState = do
--     r <- get
--     modify (+1)
--     r' <- get
--     put (r' + 10)

boolType :: QQ
boolType =
    Let "bool"
        (Alternative (Cons "false" []) (Cons "true" []))
        (Var "bool")

falseOrTrue :: QQ
falseOrTrue = Alternative (Cons "false" []) (Cons "true" [])

falseValue :: QQ
falseValue = Cons "false" []

l1 = Cons "cons" [Alternative (Cons "false" []) (Cons "true" []),Cons "nil" []]

-- listType = Let "list" $
--     Alternative
--         (Cons "nil" [])
--         (Cons "cons" [])

{- Interpret as generator

>>> pvs $ generate falseValue
["false "]

>>> pvs $ generate falseOrTrue
["false ","true "]

>>> pvs $ generate l1
["cons (false ) (nil )","cons (true ) (nil )"]

>>> generate boolType
BAD Let "bool" (Alternative (Cons "false" []) (Cons "true" [])) (Var "bool")
-}
generate :: QQ -> [Value]
generate (Cons n [])       = [Value n []]
generate (Cons n ps)       = Value n <$> mapM generate ps
generate (Alternative a b) = generate a ++ generate b
generate q                 = error $ "BAD " <> show q

data Value = Value Text [Value] deriving Show

pvs = map pv

pv (Value n ps) = n <> " " <> T.unwords (map (par . pv) ps)

par n = "(" <> n <> ")"

-- The textual representation
-- the result of parsing the textual representation
-- vs The internal, bit-based serialised representation
data QQ =
    Cons Text [QQ] -- nil ,  true , cons false nil ,..
    | Alternative QQ QQ
    | Unification QQ QQ
    | Let Text QQ QQ
    | Var Text
    -- | Let QQ QQ
       deriving Show

{-
>>> pp boolType
"(bool = ((false ) | (true )))"
-}
pp :: QQ -> Text
pp (Cons s vs)       = "(" <> s <> " " <> T.unwords (map pp vs) <> ")"
pp (Alternative a b) = "(" <> pp a <> " | " <> pp b <> ")"
pp (Unification a b) = "(" <> pp a <> " & " <> pp b <> ")"
pp (Let a b l)       = "(" <> a <> " = " <> pp b <> "in" <> pp l <> ")"
pp (Var v)           = "?" <> v
