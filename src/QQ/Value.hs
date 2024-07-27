{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module QQ.Value (
    Env,
    Value (..),
    solveVar,
    lookupVar,
    -- lookupVariable,
    Prim (..),
    Lambda (..),
    PatLambda (..),
    toValue,
    patternSymbols,
    -- apply,
    err,
    traceWith,
) where

import Data.Maybe
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Debug.Trace
import ZM.Parser.Exp
import ZM.Parser.Literal

traceWith :: (a -> String) -> a -> a
-- traceWith f a = trace (f a) a
traceWith f a = a

data Value
    = VApp Value Value
    | VEnv Env -- A list of possibly mutually recursive definitions
    | VVar Text Int
    | VMatch Text -- Matching symbol, e.g. "x"
    | VWild -- Matching wildcard, e.g. _ _aWildcard
    | Let Env Value
    | {-
      A Case is a simple form of pattern matching, where patterns are simple (not nested) and exhaustive (no guarantees of this)
      -}

      -- | VLazy Env Exp -- = (\() a) ()
      VCase [PatLambda] -- Should the lambdas' env, as they are equal?, be moved to VCase?
    | VPrim Prim
    | VCon Text [Value]
    | VLit Literal
    deriving
        ( Show
        )

data Lambda = Lambda Env Value Exp deriving (Show)

data PatLambda = PatLambda
    { lamEnv :: Env -- captured environment
    , lamPat :: Value -- parameter pattern
    , lamBody :: Value
    }
    deriving (Show)

{-
Lists of symbols in pattern

>>> patternSymbols (VCon "Cons" [VMatch "x",VCon "Cons" [VMatch "y",VCon "Nil" []]])
["x","y"]

NOTE: does not check for duplicated matching symbols.
-}
patternSymbols :: Value -> [Text]
patternSymbols (VMatch p) = [p]
patternSymbols (VCon _ vs) = concatMap patternSymbols vs
patternSymbols _ = []

data Prim = Prim
    { primName :: Text
    , -- ?this should disappear in Value
      primBody :: [Value] -> Maybe Value -- Return Nothing only if parameters are not of the right type or arity
    , primArgs :: [Value]
    }

instance Show Prim where show (Prim{..}) = T.unpack $ T.concat [primName, "(", T.pack . unwords . map show $ primArgs, ")"]

class ToValue a where toValue :: a -> Value

class FromValue a where fromValue :: Value -> Maybe a

instance FromValue Int where
    fromValue (VLit (LInteger i)) = Just . fromIntegral $ i
    fromValue _ = Nothing

instance ToValue Int where toValue = VLit . LInteger . fromIntegral

-- instance (FromValue a, ToValue b) => ToValue (a -> b) where
--     toValue (name, f) = VPrim . Prim name $
--         \va -> case fromValue va of
--             Just a -> Just $ toValue $ f a
--             Nothing -> Nothing

type Env = [(Text, Value)]

-- An unambigous reference to a variable in context
data Variable = Variable Text Int deriving (Eq, Show)

solveVar name index environment =
    fromMaybe (error $ "unknown symbol " ++ unpack name) $
        traceWith (\r -> unwords [unpack name ++ show index, "in", show environment, "returns", show r]) $
            lookupVar name index environment

lookupVar :: (Show a) => Text -> Int -> [(Text, a)] -> Maybe a
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
            VVar name (negate index - 1)

err :: [String] -> c
err = error . unwords
