{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module QQ.Evaluate where

import Data.Maybe
import Data.Text (Text, unpack)
import QQ.Eval
import QQ.Value
import Text.Show.Pretty (ppShow)
import ZM.Parser.Literal

t = putStrLn . ee

ee = either id ppShow . textToEval stdEnv


{- $setup
>>> let e = error . ee
-}

{-
>>> e "1"
VLit (LInteger 1)

>>> e "(x -> x)"
VCase
  [ PatLambda
      { lamEnv =
          [ ( "addIntInt" , VPrim addIntInt () )
          , ( "subIntInt" , VPrim subIntInt () )
          ]
      , lamPat = VMatch "x"
      , lamBody = VVar "x" 0
      }
  ]

>>> e "(x -> x) X Y"
VCon "X" [ VCon "Y" [] ]

>>> e "(x -> y -> x)"
VCase
  [ PatLambda
      { lamEnv =
          [ ( "addIntInt" , VPrim addIntInt () )
          , ( "subIntInt" , VPrim subIntInt () )
          ]
      , lamPat = VMatch "x"
      , lamBody =
          VCase
            [ PatLambda
                { lamEnv =
                    [ ( "x" , VVar "x" 0 )
                    , ( "addIntInt" , VPrim addIntInt () )
                    , ( "subIntInt" , VPrim subIntInt () )
                    ]
                , lamPat = VMatch "y"
                , lamBody = VVar "x" 0
                }
            ]
      }
  ]

-}
textToEval env = fmap (eval env) . textToValue env

{- Value level evaluation -}
eval :: Env -> Value -> Value
eval env (VApp f a) =
  traceWith (\r -> unwords [show f, "$", show a, "in", show env, "returns", show r]) $
    apply f' a'
 where
  f' = eval env f
  a' = eval env a

  {-
      Lambda Pattern Application

      >>> e "{F -> T \n T -> F} Nil"

      >>> e "{F -> T \n T -> F} F"
      
    >>> e "(x -> y -> x) X Y"
    VVar "x" 0

    >>> e "(x -> y -> y) X Y"
    VCon "Y" []

  (x -> (y -> x)) 11

  let x = 11 in (y -> x) =>

  y -> 11

    >>> e "{F -> T \n T -> F} F"
    VCon "T" []

   >>> e "(x -> x) 11"
  -}
  apply (VCase branches) arg =
    let matches = mapMaybe (\l@(PatLambda{..}) -> (,l) <$> match lamPat arg) branches
     in case matches of
          [] -> err ["no matching patterns:", show branches, "applied to", show arg]
          ((binds, PatLambda{..}) : _) -> eval (lamEnv ++ env) $ Let binds lamBody
  {-
  Field accessor, env applied to a field name!

  >>> e "{& x=11 \n y=2 &} \"x\""
  VLit (LInteger 11)
  -}
  apply (VEnv venv) (VLit (LString fld)) = eval venv $ solveVar fld 0 env
  {-
  ???
  -}
  -- apply (VLazy env f) a = apply (eval env f) a
  {- Constructor application

  Accumulate evaluated parameters (strict constructor).

  >>> e "Cons (addIntInt 11 22) Nil"
  VCon "Cons" [ VLit (LInteger 33) , VCon "Nil" [] ]

  TODO: use SnocList
  -}
  apply (VCon n cs) a = VCon n $ cs ++ [a]
  {-
  Primitive application, accumulate evaluated parameters till it applies (BUG!)

  >>> e "addIntInt"
  VPrim addIntInt ()

  >>> e "addIntInt 11"
  VPrim addIntInt (VLit (LInteger 11))

  >>> e "addIntInt 11 22"
  VLit (LInteger 33)

  >>> e "addIntInt 11 $a"

  >>> e "addIntInt 1 ((subIntInt 1) 2)"
  VLit (LInteger 0)

  ?What about sections?

  TODO: use SnocList

  Incorrect, can be applied to more parameters than the prim's arity
  -}
  apply (VPrim p@(Prim _ prim args)) a =
    let args' = args ++ [a]
     in -- NOTE: as we do not know the arity and the type of the primitive we just apply it and see if it works
        case prim args' of
          Just r -> r
          Nothing -> VPrim $ p{primArgs = args'}
  {- Accept/suspend all other applications!

  >>> e "1 2"
  VApp (VLit (LInteger 1)) (VLit (LInteger 2))
  -}
  apply a b = VApp a b

{-
>>> e "fix (fib -> {0->0 \n 1->1 \n n-> addIntInt (fib (subIntInt n 1)) (fib (subIntInt n 2))} )"  

>>> e "fix (this -> {& x=1 \ny=this  \"x\" &}) \"x\" "
-}
eval env (VFix f) = eval env $ VApp f (VFix f)
eval env (Let binds e) = eval (binds ++ env) e
eval env (VVar n i) = solveVar n i env
eval env (VCase branches) = VCase $ map (\b -> b{lamEnv = env ++ lamEnv b}) branches
eval _ v = v

{- Pattern matching
>>> match (VCon "T" []) (VCon "T" [])
Just []

>>> match (VMatch "x") (VCon "T" [])
Just [("x",VCon "T" [])]
-}
match :: Value -> Value -> Maybe [(Text, Value)]
match p s =
  case (p, s) of
    (VCon c cs, VCon c' cs') | c == c' && length cs == length cs' -> mconcat $ Just [] : zipWith match cs cs'
    (VMatch v, o) -> Just [(v, o)]
    (VWild, _) -> Just []
    (VApp f1 a1, VApp f2 a2) -> (++) <$> match f1 f2 <*> match a1 a2 -- ?needed
    (VLit l, VLit l') | l == l' -> Just []
    _ -> Nothing
