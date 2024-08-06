{-# LANGUAGE OverloadedStrings #-}

module Grace.Test where

import Control.Exception (Exception (displayException))
import qualified Control.Monad.Except as Except
import Data.Bifunctor (bimap)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import qualified Grace.Interpret as Interpret
import Grace.Location (Location)
import Grace.Monotype ()
import qualified Grace.Normalize as Normalize
import Grace.Pretty (toText)
import Grace.Syntax ()
import Grace.Type (Type)
import Grace.Value (Value)

{-
>>> v "let not = merge { False: \\_ -> True {}, True: \\_ -> False {}} in [not (False {}),not (False {})]"
[ True
  { }
, True
  { }
]
List (fromList [Application (Alternative "True") (Record (fromList [])),Application (Alternative "True") (Record (fromList []))])
:
forall (b : Alternatives) .
  List
  < True:
    { }
\| False:
    { }
\| b
>

>>> v "(merge { False: \\_ -> True {}, True: \\_ -> False {}}) (False {})"
True
  { }
Application (Alternative "True") (Record (fromList []))
:
forall (b : Alternatives) .
  < True:
    { }
\| False:
    { }
\| b
>

>>> v "False"
False
Alternative "False"
:
forall (a : Type) .
forall (b : Alternatives) .
  a ->
  < False:
    a
\| b
>

>>> v "(\\x -> x+1) 10"
11
Scalar (Natural 11)
:
Natural

>>> v "let not = merge {True: \\_ -> False {},False: \\_ -> True {}} in  [not (False {}),not (True {})]"
[ True
  { }
, False
  { }
]
List (fromList [Application (Alternative "True") (Record (fromList [])),Application (Alternative "False") (Record (fromList []))])
:
forall (a : Alternatives) .
  List
  < True:
    { }
\| False:
    { }
\| a
>

>>> v "if true then 1 else 2"
1
Scalar (Natural 1)
:
Natural

>>> v "{x:1,y:2}.x"
1
Scalar (Natural 1)
:
Natural

>>> v "{x:1,y:2}.z"
Record type mismatch
The following record type:
  { x: Natural, y: Natural }
..:1:1:
  │
1 │ {x:1,y:2}.z
  │ ↑
… is not a subtype of the following record type:
  { z: a?, b? }
..:1:11:
  │
1 │ {x:1,y:2}.z
  │           ↑
The latter record has the following extra fields:
• z

>>> v "\"a\" + \"bc\""
"abc"
Scalar (Text "abc")
:
Text
-}

{-
>>> v "x"
Unbound variable: x
..:1:1: 
  │
1 │ x
  │ ↑

>>> v "3+4"
7
Scalar (Natural 7)
:
Natural
-}
v :: Text -> IO b
v i = do
  er <- eval i
  error $ case er of
    Right (syntax, value, typ) -> T.unpack $ T.concat [toText syntax, "\n", T.pack . show $ value, "\n:\n", toText typ]
    Left err -> displayException err

-- t :: Text -> IO (Either Interpret.InterpretError (Type Location, Value))
eval input = do
  er <- Except.runExceptT (Interpret.interpret $ Interpret.Code ".." input)
  return $ case er of
    Right (inferred, value) -> let syntax = Normalize.quote [] value in Right (syntax, value, inferred)
    Left err -> Left err
