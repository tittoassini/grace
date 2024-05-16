{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Verse.Lexer
    ( -- * Lexer
      Token(..)
    , LocatedToken(..)
    , lex
      -- , reserved
      -- * Miscellaneous
      -- , validRecordLabel
      -- , validAlternativeLabel
      -- * Errors related to parsing
    , ParseError(..)
    ) where

import           Data.Text (Text)
import           Grace.Lexer (ParseError(..))
import qualified Grace.Lexer as L
import           Grace.Location (Offset)
import           Prelude hiding (lex)

{-
>>> L.lex "" $ last tests
Right [LocatedToken {token = Label "\955", start = 0},LocatedToken {token = Label "one", start = 2},LocatedToken {token = Label "two", start = 6},LocatedToken {token = Dot, start = 9},LocatedToken {token = Label "one", start = 11}]
-}

-- t :: Either L.ParseError [L.LocatedToken]

{-
..
-}
t = mapM_ test tests

tests = [
    "="
    ,"\"abc\""
    ,"<a,b>"
    ,"3"
    ,"exists x. x"
    ,"exists x y. x"
    ,"exists xy z. xy = <2, z>"
    ,"first(x,y)"
    ,"3 | 7"
    ,"<3, x>"
    ,"x = fail"
    ,"\\x"
    ,"append<xr, ys>"
    ,"loop<>"
    ,"one{fail}"    
    ,"∃k"
    ,":="
    ,";"
    ,"all{∃i. x :=xs(i); one{p(x)}; x}"
    ,"∃i. (i =0; 10) | (i =1; 27) | (i =2; 32)"
    ,"fail"
    ,"one"
    ,"all"
    ,"-"
    -- ,"λ one two. one"
    ]

test :: Text -> IO ()
test code =
    let os = L.lex "verse" code
    in print code >> print os >> print (map loc <$> os)

lex :: String
    -- ^ Name of the input (used for error messages)
    -> Text
    -- ^ Source code
    -> Either ParseError [LocatedToken]
lex name code = map loc <$> L.lex name code

data Token =
      Add
    | All
    | Assign
    | Bar
    | CloseAngle
    | CloseBrace
    | CloseParenthesis
    | Comma 
    | Dash
    | Dot
    | Equals
    | Exists
    | Fail
    | Gt 
    | Int Int
    | Label Text
    | Lambda
    | One 
    | OpenAngle
    | OpenBrace
    | OpenParenthesis
    | SemiColon
    | TextLiteral Text
    deriving (Show,Eq)

tok :: L.Token -> Token
tok = \case
    L.Add -> Add
    L.All -> All
    L.Assign -> Assign
    L.Bar -> Bar
    L.CloseAngle -> CloseAngle
    L.CloseBrace -> CloseBrace
    L.CloseParenthesis -> CloseParenthesis
    L.Comma -> Comma
    L.Dash -> Dash
    L.Dot -> Dot
    L.Equals -> Equals
    L.Exists -> Exists
    L.Fail -> Fail
    L.Gt -> Gt
    L.Int i -> Int i
    L.Label l -> Label l
    L.Lambda -> Lambda
    L.One -> One
    L.OpenAngle -> OpenAngle
    L.OpenBrace -> OpenBrace
    L.OpenParenthesis -> OpenParenthesis
    L.SemiColon -> SemiColon
    L.TextLiteral l -> TextLiteral l
    l -> error $ "not expected: " ++ show l

data LocatedToken = LocatedToken { 
    token :: Token
    ,start :: Offset 
    } deriving (Show)

loc :: L.LocatedToken -> LocatedToken
loc lt = LocatedToken (tok (L.token lt)) (L.start lt)
