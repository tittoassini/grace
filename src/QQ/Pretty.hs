{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}
module QQ.Pretty
    ( module Grace.Pretty
    , Doc
    , AnsiStyle
    ) where

import Grace.Pretty
import Prelude hiding (GT)
import Prettyprinter (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle)
import QQ.Core

instance Pretty Value where
    pretty :: Value -> Doc AnsiStyle
    pretty (Con c)    = constructor (pretty c)

constructor = builtin