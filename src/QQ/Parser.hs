
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module QQ.Parser where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import ZM.Parser.Literal
import ZM.Parser
import ZM.Parser.Types(F(..))
import qualified ZM.Parser as P 
import Grace.Pretty
import Data.Maybe (fromMaybe)
import Grace.Type (prettyTextLiteral)

{-
>>> t "numbers"
-}
tt fileName = do
    src <- T.readFile $ concat ["qq-src/",fileName,".qq"]
    T.putStrLn src
    let r = toText . P.unAnn <$> P.parseMaybe P.mdl src
    T.putStrLn (fromMaybe "Nothing" r)

{-
>>> pp = maybe "Nothing" (show . pretty) . P.parseMaybe P.mdl 

>>> pp "3"
-}

instance (Pretty (f (F f))) => Pretty (F f) where pretty (F f) = pretty f


instance Pretty (ExpR r) where pretty (Lit l) = pretty l

instance Pretty Literal where
    pretty (LInteger n) = scalar (pretty n) 
    pretty (LFloat n) = scalar (pretty n) 
    pretty (LChar c) = pretty ['?',c] 
    pretty (LString t) = prettyTextLiteral t
