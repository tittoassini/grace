{-# LANGUAGE OverloadedStrings #-}

module Grace.Test where

import qualified Control.Monad.Except as Except
import Data.Text (Text, unpack)
import qualified Grace.Interpret as Interpret
import Grace.Location (Location)
import Grace.Monotype ()
import Grace.Pretty (toText)
import Grace.Syntax ()
import Grace.Type (Type)
import Grace.Value (Value)

{-
>>> v "let not = merge { False: \\_ -> True {}, True: \\_ -> False {}} in [not (False {}),not (False {})]"

>>> v "(merge { False: \\_ -> True {}, True: \\_ -> False {}}) (False {})"
forall (b : Alternatives) .
  < True:
    { }
\| False:
    { }
\| b
>

>>> t "False"
forall (a : Type) .
forall (b : Alternatives) .
  a ->
  < False:
    a
\| b
>

>>> t "(\\x -> x+1) 10"
Right (Scalar {location = Location {name = "..", code = "(\\x -> x+1) 10", offset = 1}, scalar = Natural},Scalar (Natural 11))
-}

v :: Text -> IO b
v i = r i >>= error . unpack . toText . fst

r :: Text -> IO (Type Location, Value)
r = fmap (\(Right a) -> a) . t

t :: Text -> IO (Either Interpret.InterpretError (Type Location, Value))
t input = Except.runExceptT (Interpret.interpret $ Interpret.Code ".." input)
