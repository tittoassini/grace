-- see https://theory.stanford.edu/~aiken/publications/papers/popl94.pdf
{-# LANGUAGE OverloadedStrings     #-}
module QQ.ConditionalTypes where

import Data.Text

-- >>> Var "a"
data Pattern = Var Text deriving Show