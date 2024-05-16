{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Verse.Pretty
    ( module Grace.Pretty
    , Doc
    , AnsiStyle
    ) where

import Grace.Pretty
import Prelude hiding (GT)
import Prettyprinter (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle)
import Verse.Core

instance Pretty [Value] where 
    -- pretty = prettyValues "[" "]"
    pretty = sepBy ","

prettyValues :: ( Pretty a) => Doc AnsiStyle -> Doc AnsiStyle -> [a] -> Doc AnsiStyle
-- prettyValues beg end [] = punctuation beg <> punctuation end
prettyValues beg end vs = punctuation beg <> sepBy "," vs <> punctuation end

sepBy :: Pretty a => Doc AnsiStyle -> [a] -> Doc AnsiStyle
sepBy _ [] = ""
sepBy s (v:vs) = pretty v <> foldMap (\e -> punctuation s <> pretty e) vs

instance Pretty Exp where
    pretty Fail           = keyword "fail"
    pretty (One e)        = keyword "one" <> pars "{" "}" e
    pretty (All e)        = keyword "all" <> pars "{" "}" e
    pretty (Exists v e)   = punctuation "∃" <> pretty v <> punctuation "." <> pretty e -- pars "(" ")" e
    pretty (Seq eq e)     = pretty eq <> punctuation ";" <> pretty e
    pretty (Choice e1 e2) = pretty e1 <> " " <> operator "|" <> " " <> pretty e2
    pretty (App f v)      = pretty f <> " " <> pretty v
    pretty (Val v)        = pretty v
    pretty (Loc _ e)      = pretty e


-- pars :: Pretty a => Cha -> Doc AnsiStyle -> a -> Doc AnsiStyle
-- pars :: Pretty a => Char -> Char -> a -> Doc AnsiStyle
pars beg end v = beg <> pretty v <> end

instance Pretty ExpOrEq where
    pretty (Exp e)  = pretty e
    pretty (Eq v e) = pretty v <> punctuation "=" <> pretty e

instance Pretty Value where
    pretty (Var v) = pretty v
    pretty (HNF h) = pretty h
    pretty (LocV _ v) = pretty v

instance Pretty HNF where
    pretty (Lit l)    = pretty l
    pretty (Op p)     = pretty p
    pretty (Tuple vs) = prettyValues "<" ">" vs
    pretty (Lam v e)  = punctuation "\\" <> label (pretty v) <> punctuation "->" <> pretty e

instance Pretty Op where
    pretty GT  = operator "gt"
    pretty Add = operator "add"

{-
>>> pretty (Int 11)
11

>>> pretty (Str "abc")
abc
-}
instance Pretty Lit where
        pretty (Int i) = scalar $ pretty i
        pretty (Str s) = scalar $ pretty (show s)
