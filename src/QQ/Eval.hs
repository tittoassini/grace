{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module QQ.Eval (
  -- * Normalization
  stdEval,
  stdEnv,
  value,
  textToValue,
  -- , quote
) where

import Data.Bifunctor (Bifunctor (..), first)
import Data.Maybe
import Data.Text (Text, unpack)
import qualified Data.Text as T
import QQ.Value
import Text.Show.Pretty (ppShow)
import ZM.Parser hiding (Value, value)

{- | Evaluate an expression, leaving behind a `Value` free of reducible
    sub-expressions

    This function uses separate types for the input (i.e. `Syntax`) and the
    output (i.e. `Value`) in order to avoid wastefully valueuating the same
    sub-expression multiple times.

let e src = ppShow . value [] <$> parseMdl src

>>> e "11"

>>> e "Cons 1 Nil"
Right "VCon \"Cons\" [VLit (LInteger 1),VCon \"Nil\" []]"

>>> parseMdl "{& x=1 \n&}"
Right (Ann 0 (Arr (Bracket {open = '{', close = '}', op = Just "&", values = [Ann 3 (InfixApp (Ann 3 (Prefix "x")) "=" (Ann 5 (Lit (LInteger 1))))]})))

>>> e "{x -> y -> y} T F"
Right "VCon \"F\" []"

>>> e "{Cons h h -> h}(Cons 1 Nil)"
Right "VLit (LInteger 1)"

>>> e "1 2"
Right "VApp (VLit (LInteger 1)) (VLit (LInteger 2))"


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

strict/lazy

quote/unquote

fix f = f '(fix f)
fact = \fact n -> if n == 0 then 1 else fact (n - 1) * n

ifte F _ y = y
ifte T x _ = x
ifthenelse = {
    F -> {_ -> {y -> y)}
    T -> {x -> {_ -> x}}
}
-}

-- t f = loadMdl $ concat ["qq-src/", f, ".qq"]
e = p

p src = error $ either id (ppShow . stdEval) (parseMdl src)

textToValue :: Env -> Text -> Either String Value
textToValue env = fmap (value env) . parseMdl

stdEval = value stdEnv

{-
>>> p "(addIntInt 1) 2"
VApp
  (VApp (VPrim addIntInt ()) (VLit (LInteger 1))) (VLit (LInteger 2))

>>> e "(addIntInt 1) ?a"
VApp
  (VApp (VPrim addIntInt ()) (VLit (LInteger 1))) (VLit (LChar 'a'))

>>> e "(addIntInt 1) j"
VApp (VApp (VPrim addIntInt ()) (VLit (LInteger 1))) (VMatch "j")
-}
stdEnv :: Env
stdEnv = primitives

value ::
  -- | Evaluation environment (starting at @[]@ for a top-level expression)
  Env ->
  -- | Surface syntax
  Exp ->
  -- | Result, free of reducible sub-expressions
  Value
value env (Ann _ (App f a)) =
  -- apply f' a'
  VApp f' a'
 where
  f' = value env f
  a' = value env a

{-

pat -> body

>>> p "(x -> y -> addIntInt x y) 11 22"
VApp
  (VApp
     (VCase
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
                      , lamBody =
                          VApp (VApp (VPrim addIntInt ()) (VVar "x" 0)) (VVar "y" 0)
                      }
                  ]
            }
        ])
     (VLit (LInteger 11)))
  (VLit (LInteger 22))

>>> p "(x -> y -> x) X Y"
VApp
  (VApp
     (VCase
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
        ])
     (VCon "X" []))
  (VCon "Y" [])

>>> p "(x -> y -> y) X Y"
VApp
  (VApp
     (VCase
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
                      , lamBody = VVar "y" 0
                      }
                  ]
            }
        ])
     (VCon "X" []))
  (VCon "Y" [])

>>> p "(_ -> y -> y) A B"
VApp
  (VApp
     (VCase
        [ PatLambda
            { lamEnv =
                [ ( "addIntInt" , VPrim addIntInt () )
                , ( "subIntInt" , VPrim subIntInt () )
                ]
            , lamPat = VWild
            , lamBody =
                VCase
                  [ PatLambda
                      { lamEnv =
                          [ ( "addIntInt" , VPrim addIntInt () )
                          , ( "subIntInt" , VPrim subIntInt () )
                          ]
                      , lamPat = VMatch "y"
                      , lamBody = VVar "y" 0
                      }
                  ]
            }
        ])
     (VCon "A" []))
  (VCon "B" [])

OOPS
>>> p "(x -> y)"
VCase
  [ PatLambda
      { lamEnv =
          [ ( "addIntInt" , VPrim addIntInt () )
          , ( "subIntInt" , VPrim subIntInt () )
          ]
      , lamPat = VMatch "x"
      , lamBody = VMatch "y"
      }
  ]

>>> p "(x -> x)"
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

>>> e "(x -> x) 33"
VApp
  (VCase
     [ PatLambda
         { lamEnv =
             [ ( "addIntInt" , VPrim addIntInt () )
             , ( "subIntInt" , VPrim subIntInt () )
             ]
         , lamPat = VMatch "x"
         , lamBody = VVar "x" 0
         }
     ])
  (VLit (LInteger 33))
-}
value env (Ann _ (InfixApp p "->" r)) =
  let
    pat = valuePattern p
    env' = addLocalNames env (patternSymbols pat)
    vbody = value env' r
   in
    VCase [PatLambda env pat vbody]
{-
>>> e "((F -> T) | (T -> F)) T"
-}
value env (Ann _ (InfixApp l1 "|" l2)) =
  case (value env l1, value env l2) of
    (VCase lam1, VCase lam2) -> VCase (lam1 ++ lam2)
    _ -> error "not a lambda"
value env (Ann _ (InfixApp a op b)) =
  -- apply (apply op' a') b'
  VApp (VApp op' a') b'
 where
  op' = solveVar op 0 env
  a' = value env a
  b' = value env b

-- value env (Ann _ (Infix name)) = solveVar name 0 env

{- Constructor

>>> e "False"
Right "VCon \"False\" []"
-}
value _ (Ann _ (Con name)) = VCon name []
{-
Matching symbol in a pattern?
-}
value env (Ann _ (Prefix name)) =
  case lookupVar name 0 env of
    Just v -> v
    Nothing -> VMatch name
value _ (Ann _ (Wild name)) = VWild
{- Lists

>>> e "[]"
Right "VCon \"Nil\" []"

>>> e "[11 \n22]"
Right "VCon \"Cons\" [VLit (LInteger 11),VCon \"Cons\" [VLit (LInteger 22),VCon \"Nil\" []]]"

-}
value env (Ann _ (Arr (Bracket{open = '[', close = ']', op = Nothing, values = vs}))) =
  -- foldr (\v l -> VApp (VApp (VCon "::") (value env v)) l) (VCon "Nil" []) vs
  foldr (\v l -> VCon "Cons" [value env v, l]) (VCon "Nil" []) vs
{-
Block of possibly (mutually) recursive definitions/Module/Record/Environment

>>> e "{& x=1 \n y=2 &}"
VEnv
  [ ( "y" , VLit (LInteger 2) )
  , ( "x" , VLit (LInteger 1) )
  , ( "addIntInt" , VPrim addIntInt () )
  , ( "subIntInt" , VPrim subIntInt () )
  ]

>>> e "{& x=2 \n y= x &} \"y\""
VApp
  (VEnv
     [ ( "y" , VVar "x" 0 )
     , ( "x" , VLit (LInteger 2) )
     , ( "addIntInt" , VPrim addIntInt () )
     , ( "subIntInt" , VPrim subIntInt () )
     ])
  (VLit (LString "y"))

>>> e "{& x=subIntInt 2 z  \n z=3 &}"
VEnv
  [ ( "z" , VLit (LInteger 3) )
  , ( "x"
    , VApp (VApp (VPrim subIntInt ()) (VLit (LInteger 2))) (VVar "z" 0)
    )
  , ( "addIntInt" , VPrim addIntInt () )
  , ( "subIntInt" , VPrim subIntInt () )
  ]

>>> e "{& x=subIntInt z 2 \n y= addIntInt x 1 \n z=3 &}"
VEnv
  [ ( "z" , VLit (LInteger 3) )
  , ( "y"
    , VApp (VApp (VPrim addIntInt ()) (VVar "x" 0)) (VLit (LInteger 1))
    )
  , ( "x"
    , VApp (VApp (VPrim subIntInt ()) (VVar "z" 0)) (VLit (LInteger 2))
    )
  , ( "addIntInt" , VPrim addIntInt () )
  , ( "subIntInt" , VPrim subIntInt () )
  ]

>>> e "{&\n not={F->T \n T ->F} \n e={& r=not(not T) \n&} \n&}"
VEnv
  [ ( "e"
    , VEnv
        [ ( "r" , VApp (VVar "not" 0) (VApp (VVar "not" 0) (VCon "T" [])) )
        , ( "e" , VVar "e" 0 )
        , ( "not" , VVar "not" 0 )
        , ( "addIntInt" , VPrim addIntInt () )
        , ( "subIntInt" , VPrim subIntInt () )
        ]
    )
  , ( "not"
    , VCase
        [ PatLambda
            { lamEnv =
                [ ( "e" , VVar "e" 0 )
                , ( "not" , VVar "not" 0 )
                , ( "addIntInt" , VPrim addIntInt () )
                , ( "subIntInt" , VPrim subIntInt () )
                ]
            , lamPat = VCon "F" []
            , lamBody = VCon "T" []
            }
        , PatLambda
            { lamEnv =
                [ ( "e" , VVar "e" 0 )
                , ( "not" , VVar "not" 0 )
                , ( "addIntInt" , VPrim addIntInt () )
                , ( "subIntInt" , VPrim subIntInt () )
                ]
            , lamPat = VCon "T" []
            , lamBody = VCon "F" []
            }
        ]
    )
  , ( "addIntInt" , VPrim addIntInt () )
  , ( "subIntInt" , VPrim subIntInt () )
  ]

>>> e "{&\n x=1 \n y=2 \n f={T->x \n F->y\n} \n f1=f T \n f2=f F\n&} \"x\""
VApp
  (VEnv
     [ ( "f2" , VApp (VVar "f" 0) (VCon "F" []) )
     , ( "f1" , VApp (VVar "f" 0) (VCon "T" []) )
     , ( "f"
       , VCase
           [ PatLambda
               { lamEnv =
                   [ ( "f2" , VVar "f2" 0 )
                   , ( "f1" , VVar "f1" 0 )
                   , ( "f" , VVar "f" 0 )
                   , ( "y" , VVar "y" 0 )
                   , ( "x" , VVar "x" 0 )
                   , ( "addIntInt" , VPrim addIntInt () )
                   , ( "subIntInt" , VPrim subIntInt () )
                   ]
               , lamPat = VCon "T" []
               , lamBody = VVar "x" 0
               }
           , PatLambda
               { lamEnv =
                   [ ( "f2" , VVar "f2" 0 )
                   , ( "f1" , VVar "f1" 0 )
                   , ( "f" , VVar "f" 0 )
                   , ( "y" , VVar "y" 0 )
                   , ( "x" , VVar "x" 0 )
                   , ( "addIntInt" , VPrim addIntInt () )
                   , ( "subIntInt" , VPrim subIntInt () )
                   ]
               , lamPat = VCon "F" []
               , lamBody = VVar "y" 0
               }
           ]
       )
     , ( "y" , VLit (LInteger 2) )
     , ( "x" , VLit (LInteger 1) )
     , ( "addIntInt" , VPrim addIntInt () )
     , ( "subIntInt" , VPrim subIntInt () )
     ])
  (VLit (LString "x"))

>>> e "{& \n l=len Nil \n len = {\nNil -> Z\nCons h t -> S (len t)}\n&} \"l\""
VApp
  (VEnv
     [ ( "len"
       , VCase
           [ PatLambda
               { lamEnv =
                   [ ( "len" , VVar "len" 0 )
                   , ( "l" , VVar "l" 0 )
                   , ( "addIntInt" , VPrim addIntInt () )
                   , ( "subIntInt" , VPrim subIntInt () )
                   ]
               , lamPat = VCon "Nil" []
               , lamBody = VCon "Z" []
               }
           , PatLambda
               { lamEnv =
                   [ ( "len" , VVar "len" 0 )
                   , ( "l" , VVar "l" 0 )
                   , ( "addIntInt" , VPrim addIntInt () )
                   , ( "subIntInt" , VPrim subIntInt () )
                   ]
               , lamPat = VApp (VApp (VCon "Cons" []) (VMatch "h")) (VMatch "t")
               , lamBody = VApp (VCon "S" []) (VApp (VVar "len" 0) (VMatch "t"))
               }
           ]
       )
     , ( "l" , VApp (VVar "len" 0) (VCon "Nil" []) )
     , ( "addIntInt" , VPrim addIntInt () )
     , ( "subIntInt" , VPrim subIntInt () )
     ])
  (VLit (LString "l"))

>>> e "{& \n l=len Nil \n len = {\nNil -> Z\nCons h t -> S (len t)}\n&} \"l\""
VApp
  (VEnv
     [ ( "len"
       , VCase
           [ PatLambda
               { lamEnv =
                   [ ( "len" , VVar "len" 0 )
                   , ( "l" , VVar "l" 0 )
                   , ( "addIntInt" , VPrim addIntInt () )
                   , ( "subIntInt" , VPrim subIntInt () )
                   ]
               , lamPat = VCon "Nil" []
               , lamBody = VCon "Z" []
               }
           , PatLambda
               { lamEnv =
                   [ ( "len" , VVar "len" 0 )
                   , ( "l" , VVar "l" 0 )
                   , ( "addIntInt" , VPrim addIntInt () )
                   , ( "subIntInt" , VPrim subIntInt () )
                   ]
               , lamPat = VApp (VApp (VCon "Cons" []) (VMatch "h")) (VMatch "t")
               , lamBody = VApp (VCon "S" []) (VApp (VVar "len" 0) (VMatch "t"))
               }
           ]
       )
     , ( "l" , VApp (VVar "len" 0) (VCon "Nil" []) )
     , ( "addIntInt" , VPrim addIntInt () )
     , ( "subIntInt" , VPrim subIntInt () )
     ])
  (VLit (LString "l"))

>>> e "{&\nl=len (Cons 1 (Cons 2 Nil))\nlen = {\nNil -> Z\nCons h t -> S (len t)}\n&} \"l\""
VApp
  (VEnv
     [ ( "len"
       , VCase
           [ PatLambda
               { lamEnv =
                   [ ( "len" , VVar "len" 0 )
                   , ( "l" , VVar "l" 0 )
                   , ( "addIntInt" , VPrim addIntInt () )
                   , ( "subIntInt" , VPrim subIntInt () )
                   ]
               , lamPat = VCon "Nil" []
               , lamBody = VCon "Z" []
               }
           , PatLambda
               { lamEnv =
                   [ ( "len" , VVar "len" 0 )
                   , ( "l" , VVar "l" 0 )
                   , ( "addIntInt" , VPrim addIntInt () )
                   , ( "subIntInt" , VPrim subIntInt () )
                   ]
               , lamPat = VApp (VApp (VCon "Cons" []) (VMatch "h")) (VMatch "t")
               , lamBody = VApp (VCon "S" []) (VApp (VVar "len" 0) (VMatch "t"))
               }
           ]
       )
     , ( "l"
       , VApp
           (VVar "len" 0)
           (VApp
              (VApp (VCon "Cons" []) (VLit (LInteger 1)))
              (VApp (VApp (VCon "Cons" []) (VLit (LInteger 2))) (VCon "Nil" [])))
       )
     , ( "addIntInt" , VPrim addIntInt () )
     , ( "subIntInt" , VPrim subIntInt () )
     ])
  (VLit (LString "l"))

>>> p "{&\nlen = {\nNil -> Z\nCons h t -> S (len t)}\n&}"
VEnv
  [ ( "len"
    , VCase
        [ PatLambda
            { lamEnv =
                [ ( "len" , VVar "len" 0 )
                , ( "addIntInt" , VPrim addIntInt () )
                , ( "subIntInt" , VPrim subIntInt () )
                ]
            , lamPat = VCon "Nil" []
            , lamBody = VCon "Z" []
            }
        , PatLambda
            { lamEnv =
                [ ( "len" , VVar "len" 0 )
                , ( "addIntInt" , VPrim addIntInt () )
                , ( "subIntInt" , VPrim subIntInt () )
                ]
            , lamPat = VApp (VApp (VCon "Cons" []) (VMatch "h")) (VMatch "t")
            , lamBody = VApp (VCon "S" []) (VApp (VVar "len" 0) (VMatch "t"))
            }
        ]
    )
  , ( "addIntInt" , VPrim addIntInt () )
  , ( "subIntInt" , VPrim subIntInt () )
  ]

-}
value env (Ann _ (Arr (Bracket{open = '{', close = '}', op = Just "&", values = branches}))) =
  VEnv $ env'' ++ env
 where
  env'' = zip ns $ map (value env') vs
  -- env' = map (second (VLazy env'')) binds ++ env
  env' = addLocalNames env ns
  (ns, vs) = unzip . map nameExp $ reverse branches

  nameExp (Ann _ (InfixApp (Ann _ (Prefix name)) "=" ex)) = (name, ex)
  -- nameExp (Ann _ (App (Ann _ (App (Ann _ (Infix "=")) p@((Ann _ (Prefix n))))) e)) = (n, e)
  nameExp notABranch = err ["not a definition", show notABranch]

{- General form of Lambda/Case

Shoult it be {| |} ?

>>> e "{}"
Right "VCase []"

>>> e "{} T"
no matching patterns: [] applied to VCon "T" []

>>> e "{1} T"
not a pattern matching clause: VLit (LInteger 1)

-}
value env (Ann _ (Arr (Bracket{open = '{', close = '}', op = Nothing, values = branches}))) =
  VCase $ concatMap (isLam . value env) branches
 where
  isLam (VCase ls) = ls
  isLam notABranch = err ["not a pattern matching clause:", show notABranch]

-- Generic Bracket valueuation, apply op to each branch
-- TODO: no valueuation
-- as is equivalent to a fold why not writing it as such?
value env (Ann _ (Arr (Bracket{open = '{', close = '}', op = Just oper, values = vs}))) =
  -- foldr (\v e -> VApp (VApp (VCon oper) (value env v)) e) (error "empty seq") vs
  foldr (\v e -> VCon oper [value env v, e]) (error "empty seq") vs
value _ (Ann _ (Arr b@Bracket{})) = err ["unsupported bracket:", show b]
{-
Literal valueuate to themselves

>>> e "1"
Right "VLit (LInteger 1)"
-}
value _ (Ann _ (Lit l)) = VLit l

addLocalNames :: Env -> [Text] -> [(Text, Value)]
addLocalNames env ns = map (\n -> (n, VVar n 0)) ns ++ env

{-
Semi-dynamic patterns

Symbol are valueuated to matching symbols (VMatch) and expressions are valueuated.
-}
valuePattern :: Exp -> Value
valuePattern = value []

-- | Substitute an expression into a `Closure`

-- instantiate :: Closure -> Value -> Value
-- instantiate (Closure name env syntax) value =
--     valueuate ((name, value) : env) syntax

-- data Closure = Closure [(Value, Exp)] Env deriving (Show)

primitives :: Env
primitives = map (\(n, p) -> (n, VPrim $ Prim n p [])) [("addIntInt", addIntInt), ("subIntInt", subIntInt)]

addIntInt [VLit (LInteger a), VLit (LInteger b)] = Just $ VLit $ LInteger $ a + b
addIntInt _ = Nothing

subIntInt [VLit (LInteger a), VLit (LInteger b)] = Just $ VLit $ LInteger $ a - b
subIntInt _ = Nothing
