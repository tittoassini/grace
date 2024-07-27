{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module QQ.Pretty (
    -- module Grace.Pretty,
    Pretty (..),
    Doc,
    AnsiStyle,
    prettyText,
) where

-- import Grace.Pretty

import Data.Text (pack)
import Prettyprinter (Doc, Pretty (..))
import Prettyprinter.Render.Terminal (AnsiStyle)
import Prelude hiding (GT)

-- import QQ.Core

-- instance Pretty Value where
--     pretty :: Value -> Doc AnsiStyle
--     pretty (Con c)    = constructor (pretty c)

-- constructor = builtin

prettyText = pack . show . pretty