{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module QQ.Compiler (
    compileToFile,
    stdCompile,
    compile,
) where

import Data.Bifunctor (Bifunctor (..), first)
import Data.Maybe
import Data.Text (Text, pack, singleton, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Prettyprinter
import QQ.Eval
import QQ.Pretty
import QQ.Value
import ZM.Parser hiding (Value, value)

compileToFile jsFile exp = T.writeFile jsFile $ "import rt from './runtime.mjs';\n" <> prettyText (stdCompile exp)

stdCompile :: Exp -> JValue
stdCompile = compile [] . stdEval

-- primitives :: Env
-- primitives = map (\n -> (n, VPrim $ Prim n (const Nothing) [])) ["addIntInt", "subIntInt"]

class Compile c where compile :: Env -> c -> JValue

{- $setup

c = fmap stdCompile . parseMdl
>>> c = error . unpack . stdCompile
-}

{-
>>> c (Ann 0 $ Con "True"::Exp)
["True"]

>>> c (LInteger 11)
11

>>> c (LChar 'z')
"z"

>>> c (LChar '\n')
"\n"
-}
-- instance Compile Exp where
--     compile env (Ann _ e) = comp e
--       where
--         comp (App f a) =
--             let
--                 f' = compile env f
--                 a' = compile env a
--              in
--                 case f' of
--                     JCon c ps -> JCon c (ps ++ [a'])
--                     _ -> error "unsupported application"
--         comp l@(Con c) = JCon c []
--         comp (Lit l) = JLit l
--         comp e = error $ "unsupported compilation of " ++ show e

instance Compile Value where
    compile env (VPrim (Prim{..})) = JPrim primName $ map (compile env) primArgs
    compile env (VCon c cs) = JCon c $ map (compile env) cs
    compile _ (VLit l) = JLit l
    compile _ e = error $ "unsupported compilation of " ++ show e

data JValue
    = JCase
    | JPrim Text [JValue]
    | JCon Text [JValue] -- Fully applied? constructor name and arguments
    | JLit Literal
    deriving
        ( -- | IfThenElse
          Show
        )

instance Pretty JValue where
    pretty (JPrim name args) = pretty name <> tupled (map pretty args) -- NOTE: No arity check
    pretty (JCon name args) = list $ pretty (show name) : map pretty args
    pretty (JLit (LChar c)) = pretty $ T.singleton c
    pretty (JLit l) = pretty l
