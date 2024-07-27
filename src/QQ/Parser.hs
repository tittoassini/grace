{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module QQ.Parser where

import Grace.Pretty
import Grace.Type (prettyTextLiteral)
import Prettyprinter (hsep, vcat, (<+>))
import ZM.Parser
import qualified ZM.Parser as P
import ZM.Parser.Types (F (..))

