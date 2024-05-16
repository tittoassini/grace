{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RecursiveDo       #-}

{-| This module contains the logic for parsing Grace files using @Earley@.

    The main reason for not using @attoparsec@ or @megaparsec@ is because
    LR parsers are easier to maintain due to not needing to left-factor the
    grammar.

    The main reason for not using @happy@ is because it uses a separate code
    generation step, which leads to worse type errors and poor support for
    interactive type-checking.
-}

module Verse.Parser
    ( -- * Parsing
      parse
      -- * Errors related to parsing
    , ParseError(..)
    ) where

import           Control.Applicative (many, optional, some, (<|>))
import           Control.Applicative.Combinators (endBy, sepBy)
import           Control.Applicative.Combinators.NonEmpty (sepBy1)
import           Data.Functor (void, ($>))
import           Data.List.NonEmpty (NonEmpty(..), some1)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Scientific (Scientific)
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Grace.Domain as Domain
import           Grace.Input (Input(..))
import           Grace.Location (Location(..), Offset(..))
import qualified Grace.Monotype as Monotype
import           Grace.Syntax (Binding(..), Syntax(..))
import           Prelude hiding (GT)
-- import qualified Grace.Syntax as Syntax
-- import           Grace.Type (Type(..))
-- import qualified Grace.Type as Type
import qualified Text.Earley as Earley
import           Text.Earley
    (Grammar, Prod, Report(..), namedToken, rule, (<?>))
import qualified Text.URI as URI
import           Verse.Core
import qualified Verse.Lexer as Lexer
import           Verse.Lexer (LocatedToken(LocatedToken), ParseError(..), Token)

type Parser r = Prod r Text LocatedToken

matchLabel :: Token -> Maybe Text
matchLabel (Lexer.Label l) = Just l
matchLabel  _              = Nothing

-- matchAlternative :: Token -> Maybe Text
-- matchAlternative (Lexer.Alternative a) = Just a
-- matchAlternative  _                    = Nothing

-- matchReal :: Token -> Maybe Scientific
-- matchReal (Lexer.RealLiteral n) = Just n
-- matchReal  _                    = Nothing

matchInt :: Token -> Maybe Int
matchInt (Lexer.Int n) = Just n
matchInt  _            = Nothing

matchText :: Token -> Maybe Text
matchText (Lexer.TextLiteral t) = Just t
matchText  _                    = Nothing

-- matchFile :: Token -> Maybe FilePath
-- matchFile (Lexer.File f) = Just f
-- matchFile  _             = Nothing

-- matchURI :: Token -> Maybe URI.URI
-- matchURI (Lexer.URI t) = Just t
-- matchURI  _            = Nothing

terminal :: (Token -> Maybe a) -> Parser r a
terminal match = Earley.terminal match'
  where
    match' locatedToken_ = match (Lexer.token locatedToken_)

label :: Parser r Text
label = terminal matchLabel

-- alternative :: Parser r Text
-- alternative = terminal matchAlternative

int :: Parser r Int
int = terminal matchInt

text :: Parser r Text
text = terminal matchText

token :: Token -> Parser r ()
token t = void (Earley.satisfy predicate <?> render t)
  where
    predicate locatedToken_ = Lexer.token locatedToken_ == t

locatedTerminal :: (Token -> Maybe a) -> Parser r (Offset, a)
locatedTerminal match = Earley.terminal match'
  where
    match' locatedToken_@LocatedToken{ start }  = do
      a <- match (Lexer.token locatedToken_)
      return (start, a)

locatedLabel :: Parser r (Offset, Text)
locatedLabel = locatedTerminal matchLabel

-- locatedAlternative :: Parser r (Offset, Text)
-- locatedAlternative = locatedTerminal matchAlternative

-- locatedReal :: Parser r (Offset, Scientific)
-- locatedReal = locatedTerminal matchReal

locatedInt :: Parser r (Offset, Int)
locatedInt = locatedTerminal matchInt

locatedText :: Parser r (Offset, Text)
locatedText = locatedTerminal matchText

-- locatedFile :: Parser r (Offset, FilePath)
-- locatedFile = locatedTerminal matchFile

-- locatedURI :: Parser r (Offset, URI.URI)
-- locatedURI = locatedTerminal matchURI

locatedToken :: Token -> Parser r Offset
locatedToken expectedToken =
    Earley.terminal capture <?> render expectedToken
  where
    capture LocatedToken{ Lexer.token = actualToken, .. }
        | expectedToken == actualToken = Just start
        | otherwise                    = Nothing

-- | This render function is currently never used since `Location.renderError`
--   does not display expected tokens at all, but I maintain this anyway in
--   case someone wants to modify the code to display them.
render :: Token -> Text
render = Text.pack . show

-- render t = case t of
--     Lexer.Alternative _    -> "an alternative"
--     Lexer.Alternatives     -> "Alternatives"
--     Lexer.And              -> "&&"
--     Lexer.Arrow            -> "->"
--     Lexer.Assign           -> ":="
--     Lexer.At               -> "@"
--     Lexer.Bar              -> "|"
--     Lexer.Bool             -> "Bool"
--     Lexer.CloseAngle       -> ">"
--     Lexer.CloseBrace       -> "}"
--     Lexer.CloseBracket     -> "]"
--     Lexer.CloseParenthesis -> ")"
--     Lexer.Colon            -> ":"
--     Lexer.Comma            -> ","
--     Lexer.Dash             -> "-"
--     Lexer.Dot              -> "."
--     Lexer.Real             -> "Real"
--     Lexer.RealLiteral _    -> "a real number literal"
--     Lexer.RealEqual        -> "Real/equal"
--     Lexer.RealLessThan     -> "Real/lessThan"
--     Lexer.RealNegate       -> "Real/negate"
--     Lexer.RealShow         -> "Real/show"
--     Lexer.Else             -> "else"
--     Lexer.Equals           -> "="
--     Lexer.Exists           -> "exists"
--     Lexer.False_           -> "False"
--     Lexer.Fields           -> "Fields"
--     Lexer.File _           -> "a file"
--     Lexer.Forall           -> "forall"
--     Lexer.If               -> "if"
--     Lexer.In               -> "in"
--     Lexer.Int _            -> "an integer literal"
--     Lexer.Integer          -> "Integer"
--     Lexer.IntegerAbs       -> "Integer/clamp"
--     Lexer.IntegerEven      -> "Integer/even"
--     Lexer.IntegerNegate    -> "Integer/negate"
--     Lexer.IntegerOdd       -> "Integer/odd"
--     Lexer.JSON             -> "JSON"
--     Lexer.JSONFold         -> "JSON/fold"
--     Lexer.Label _          -> "a label"
--     Lexer.Lambda           -> "\\"
--     Lexer.Let              -> "let"
--     Lexer.List             -> "list"
--     Lexer.ListDrop         -> "List/drop"
--     Lexer.ListEqual        -> "List/equal"
--     Lexer.ListFold         -> "List/fold"
--     Lexer.ListHead         -> "List/head"
--     Lexer.ListIndexed      -> "List/indexed"
--     Lexer.ListLast         -> "List/last"
--     Lexer.ListLength       -> "List/length"
--     Lexer.ListMap          -> "List/map"
--     Lexer.ListReverse      -> "List/reverse"
--     Lexer.ListTake         -> "List/take"
--     Lexer.Merge            -> "merge"
--     Lexer.Natural          -> "Natural"
--     Lexer.NaturalFold      -> "Natural/fold"
--     Lexer.Null             -> "null"
--     Lexer.OpenAngle        -> "<"
--     Lexer.OpenBrace        -> "{"
--     Lexer.OpenBracket      -> "<"
--     Lexer.OpenParenthesis  -> "("
--     Lexer.Optional         -> "List"
--     Lexer.Or               -> "||"
--     Lexer.Plus             -> "+"
--     Lexer.SemiColon        -> ";"
--     Lexer.Text             -> "Text"
--     Lexer.TextEqual        -> "Text/equal"
--     Lexer.TextLiteral _    -> "a text literal"
--     Lexer.Then             -> "then"
--     Lexer.Type             -> "Type"
--     Lexer.Times            -> "*"
--     Lexer.True_            -> "True"
--     Lexer.URI _            -> "a URI"

grammar :: Grammar r (Parser r Exp)
grammar = mdo

    expression <- rule
        (   
            do -- (e)
                token Lexer.OpenParenthesis
                e <- expression
                token Lexer.CloseParenthesis
                return e

            -- a | 11 | b
            <|> orExpression

        )

    basicExpression <- rule
        (   
                
            (\location -> Loc location Fail) <$> locatedToken Lexer.Fail

            <|> do  -- one {..}
                token Lexer.One
                token Lexer.OpenBrace
                e <- expression
                token Lexer.CloseBrace
                return $ One e

            <|> do  -- all {..}
                token Lexer.All
                token Lexer.OpenBrace
                e <- expression
                token Lexer.CloseBrace
                return $ All e

            -- Exists a b .. . e => Exists a $ Exists b ..
            <|> ctx Lexer.Exists (\loc var body -> Loc loc $ Exists var body)

            -- seq: a=b;c=b;c
            <|> seqExpression

            <|> do 
                    f <- value
                    v <- value
                    return $ App f v
            
            <|> Val <$> value 
        )

    let ctx tok con = do
                    location <- locatedToken tok
                    locatedNames <- some1 locatedLabel
                    token Lexer.Dot
                    body0 <- expression

                    return do
                        let cons (nameLocation, varName) =  con nameLocation varName
                        foldr cons body0 locatedNames

    -- operatorExpression <- rule plusExpression

    seqExpression <- rule $ do
                ee <- expOrEq
                token Lexer.SemiColon
                e <- expression
                return $ Seq ee e

    expOrEq <- rule
        (   do 
                l <- value                
                token Lexer.Equals
                r <- expression
                return (Eq l r)

                <|> Exp <$> expression
        )

    -- seqExpression <- rule (op Lexer.SemiColon Seq expOrEq)

    -- e | (e=2;b) 
    orExpression <- rule (op Lexer.Bar Choice basicExpression)

    let op token_ operator subExpression = do
            let snoc left (operatorLocation, right) =
                    Loc operatorLocation (operator left right) 

            e0 <- subExpression

            ses <- many do
                s <- locatedToken token_
                e <- subExpression;
                return (s, e)

            return (foldl snoc e0 ses)
    --  _ * (a+b) & f b | b  +  _*_ 
    -- a*b + c
    -- plusExpression <- rule (op Lexer.Plus Syntax.Plus timesExpression)

    -- timesExpression <- rule (op Lexer.Times Syntax.Times orExpression)

    -- orExpression <- rule (op Lexer.Bar Syntax.Or andExpression)

    -- andExpression <- rule (op Lexer.And Syntax.And applicationExpression)

    -- let application function argument =
    --         Syntax.Application{ location = Syntax.location function, .. }

    -- applicationExpression <- rule
    --     (   do  es <- some1 fieldExpression
    --             return (foldl application (NonEmpty.head es) (NonEmpty.tail es))
    --     <|> do  location <- locatedToken Lexer.Merge
    --             ~(handlers :| es) <- some1 fieldExpression

    --             return do
    --                 let nil = Syntax.Merge{..}
    --                 foldl application nil es
    --     )

    -- fieldExpression <- rule do
    --     let snoc record0 record (fieldLocation, field) =
    --             Syntax.Field{ location = Syntax.location record0, .. }

    --     record <- primitiveExpression
    --     fields <- many (do token Lexer.Dot; l <- locatedRecordLabel; return l)

    --     return (foldl (snoc record) record fields)

    value <- rule
        (   

        -- negative integer -33
        do  
                ~(location, n) <- token Lexer.Dash *>locatedInt
                return $ LocV location . HNF . Lit . Int . negate $ n 

        -- positive integer
        <|> do  ~(location, n) <- locatedInt
                return $ LocV location . HNF . Lit . Int $ n 

        -- add
        <|> do               
                location <- locatedToken Lexer.Add
                return $ LocV location . HNF . Op $ Add

        -- gt
        <|> do               
                location <- locatedToken Lexer.Gt
                return $ LocV location . HNF . Op $ GT

        -- Variable
        <|> do  
            ~(location, name) <- locatedLabel
            return $ LocV location $ Var name -- { index = 0, .. }

        -- String
        <|> (\(location, t) -> LocV location . HNF . Lit . Str $ t) <$> locatedText
    
        --    Tuple <e1,e2>
        <|> do  
                location <- locatedToken Lexer.OpenAngle
                optional (token Lexer.Comma)
                elements <- value `sepBy` token Lexer.Comma
                optional (token Lexer.Comma)
                token Lexer.CloseAngle                
                return $ LocV location . HNF . Tuple $ elements -- Syntax.List{ elements = Seq.fromList elements, .. }

        -- Lambda 𝜆 a b c . e => 𝜆 a $ 𝜆 b $ 𝜆 c
        -- <|> ctx Lexer.Lambda (\loc var body -> LocV loc $ HNF (Lam var body))
        <|> (\(Val v) -> v)  <$> ctx Lexer.Lambda (\loc var body -> Val (LocV loc $ HNF (Lam var body)))

        )



        -- <|> do  ~(location, name) <- locatedLabel
        --         token Lexer.At
        --         index <- int

        --         return Syntax.Variable{..}

        -- <|> do  ~(location, name) <- locatedAlternative

        --         return Syntax.Alternative{..}

 

        -- Record {field1,field2} 
        -- <|> do  location <- locatedToken Lexer.OpenBrace
        --         optional (token Lexer.Comma)
        --         fieldValues <- fieldValue `sepBy` token Lexer.Comma
        --         optional (token Lexer.Comma)
        --         token Lexer.CloseBrace

        --         return Syntax.Record{..}

        -- <|> do  location <- locatedToken Lexer.True_

        --         return Syntax.Scalar{ scalar = Syntax.Bool True, .. }

        -- <|> do  location <- locatedToken Lexer.False_

        --         return Syntax.Scalar{ scalar = Syntax.Bool False, .. }

        -- <|> do  location <- locatedToken Lexer.Null

        --         return Syntax.Scalar{ scalar = Syntax.Null, .. }

        -- <|> do  sign <- (token Lexer.Dash $> negate) <|> pure id

        --         ~(location, n) <- locatedReal

        --         return Syntax.Scalar{ scalar = Syntax.Real (sign n), .. }

        -- <|> do  location <- locatedToken Lexer.RealEqual
        --         return Syntax.Builtin{ builtin = Syntax.RealEqual, .. }

        -- <|> do  ~(location, file) <- locatedFile
        --         return Syntax.Embed{ embedded = Path file, .. }

        -- <|> do  ~(location, uri) <- locatedURI
        --         return Syntax.Embed{ embedded = URI uri, .. }

        -- -- (e)
        -- <|> do  token Lexer.OpenParenthesis
        --         e <- value
        --         token Lexer.CloseParenthesis
        --         return e
        -- )

    -- binding <- rule
    --     (   
    --         --  e1 = e2
    --         do  -- nameLocation <- locatedToken Lexer.Let                
    --             (loc,name) <- locatedLabel
    --             token Lexer.Equals
    --             assignment <- expression

    --             return . Loc loc $ Seq (Eq (Var name) assignment) undefined


    --         -- Let v = e
    --         -- do  nameLocation <- locatedToken Lexer.Let
    --         --     name <- label
    --         --     token Lexer.Equals
    --         --     assignment <- expression

    --         --     return do
    --         --         let annotation = Nothing
    --         --         Syntax.Binding{..}

    --     -- let v : A = expression
    --     -- <|> do  nameLocation <- locatedToken Lexer.Let
    --     --         name <- label
    --     --         token Lexer.Colon
    --     --         annotation <- fmap Just quantifiedType
    --     --         token Lexer.Equals
    --     --         assignment <- expression

    --     --         return Syntax.Binding{..}
    --     )

    -- recordLabel <- rule (label <|> alternative <|> text)

    -- locatedRecordLabel <- rule (locatedLabel <|> locatedText)

    -- fieldValue <- rule do
    --     field <- recordLabel
    --     token Lexer.Colon
    --     value <- expression
    --     return (field, value)

    -- domain <- rule
    --     (   do  token Lexer.Type
    --             return Domain.Type
    --     <|> do  token Lexer.Fields
    --             return Domain.Fields
    --     <|> do  token Lexer.Alternatives
    --             return Domain.Alternatives
    --     )

    -- quantifiedType <- rule do
    --     let quantify (forallOrExists, location, (typeVariableOffset, typeVariable), domain_) type_ =
    --             forallOrExists location typeVariableOffset typeVariable domain_ type_

    --     fss <- many
    --         (   do  location <- locatedToken Lexer.Forall
    --                 fs <- some do
    --                     token Lexer.OpenParenthesis
    --                     locatedTypeVariable <- locatedLabel
    --                     token Lexer.Colon
    --                     domain_ <- domain
    --                     token Lexer.CloseParenthesis
    --                     return \location_ -> quantify (Type.Forall, location_, locatedTypeVariable, domain_)
    --                 token Lexer.Dot
    --                 return (map ($ location) fs)
    --         <|> do  location <- locatedToken Lexer.Exists
    --                 fs <- some do
    --                     token Lexer.OpenParenthesis
    --                     locatedTypeVariable <- locatedLabel
    --                     token Lexer.Colon
    --                     domain_ <- domain
    --                     token Lexer.CloseParenthesis

    --                     return \location_ -> quantify (Type.Exists, location_, locatedTypeVariable, domain_)
    --                 token Lexer.Dot
    --                 return (map ($ location) fs)
    --         )
    --     t <- functionType
    --     return (foldr ($) t (concat fss))

    -- functionType <- rule do
    --     let function input output =
    --             Type.Function{ location = Type.location input, ..}

    --     ts <- applicationType `sepBy1` token Lexer.Arrow
    --     return (foldr function (NonEmpty.last ts) (NonEmpty.init ts))

    -- applicationType <- rule
    --     (   do  location <- locatedToken Lexer.List
    --             type_ <- primitiveType

    --             return Type.List{..}

    --     <|> do  location <- locatedToken Lexer.Optional
    --             type_ <- primitiveType

    --             return Type.Optional{..}

    --     <|> do  primitiveType
    --     )

    -- primitiveType <- rule
    --     (   do  location <- locatedToken Lexer.Bool
    --             return Type.Scalar{ scalar = Monotype.Bool, .. }
    --     <|> do  location <- locatedToken Lexer.Real
    --             return Type.Scalar{ scalar = Monotype.Real, .. }
    --     <|> do  location <- locatedToken Lexer.Integer
    --             return Type.Scalar{ scalar = Monotype.Integer, .. }
    --     <|> do  location <- locatedToken Lexer.JSON
    --             return Type.Scalar{ scalar = Monotype.JSON, .. }
    --     <|> do  location <- locatedToken Lexer.Natural
    --             return Type.Scalar{ scalar = Monotype.Natural, .. }
    --     <|> do  location <- locatedToken Lexer.Text
    --             return Type.Scalar{ scalar = Monotype.Text, .. }
    --     <|> do  ~(location, name) <- locatedLabel
    --             return Type.VariableType{..}
    --     <|> do  let record location fields = Type.Record{..}

    --             locatedOpenBrace <- locatedToken Lexer.OpenBrace

    --             optional (token Lexer.Comma)

    --             fieldTypes <- fieldType `endBy` token Lexer.Comma

    --             toFields <-
    --                 (   do  text_ <- recordLabel
    --                         pure (\fs -> Type.Fields fs (Monotype.VariableFields text_))
    --                 <|> do  pure (\fs -> Type.Fields fs Monotype.EmptyFields)
    --                 <|> do  f <- fieldType
    --                         pure (\fs -> Type.Fields (fs <> [ f ]) Monotype.EmptyFields)
    --                 )

    --             optional (token Lexer.Comma)

    --             token Lexer.CloseBrace

    --             return (record locatedOpenBrace (toFields fieldTypes))
    --     <|> do  let union location alternatives = Type.Union{..}

    --             locatedOpenAngle <- locatedToken Lexer.OpenAngle

    --             optional (token Lexer.Bar)

    --             alternativeTypes <- alternativeType `endBy` token Lexer.Bar

    --             toAlternatives <-
    --                 (   do  text_ <- label
    --                         return (\as -> Type.Alternatives as (Monotype.VariableAlternatives text_))
    --                 <|> do  pure (\as -> Type.Alternatives as Monotype.EmptyAlternatives)
    --                 <|> do  a <- alternativeType
    --                         return (\as -> Type.Alternatives (as <> [ a ]) Monotype.EmptyAlternatives)
    --                 )

    --             optional (token Lexer.Bar)

    --             token Lexer.CloseAngle
    --             return (union locatedOpenAngle (toAlternatives alternativeTypes))
    --     <|> do  token Lexer.OpenParenthesis
    --             t <- quantifiedType
    --             token Lexer.CloseParenthesis
    --             return t
    --     )

    -- fieldType <- rule do
    --     field <- recordLabel
    --     token Lexer.Colon
    --     t <- quantifiedType
    --     return (field, t)

    -- alternativeType <- rule do
    --     a <- alternative
    --     token Lexer.Colon
    --     t <- quantifiedType
    --     return (a, t)

    return expression

{- Parse a complete expression
>>> p = parse ""

>>> p "(fail)" 
Right (Loc 1 Fail)

>>> p "fail" 
Right (Loc 0 Fail)

>>> p "exists a b. fail"
Right (Loc 7 (Exists "a" (Loc 9 (Exists "b" (Loc 12 Fail)))))

>>> p "∃ a b. fail"
Right (Loc 2 (Exists "a" (Loc 4 (Exists "b" (Loc 7 Fail)))))

>>> p "a"
Right (Val (LocV 0 (Var "a")))

>>> p "1"
Right (Val (LocV 0 (HNF (Lit (Int 1)))))

>>> p "-2"
Right (Val (LocV 1 (HNF (Lit (Int (-2))))))

>>> p "\"abc\""
Right (Val (LocV 0 (HNF (Lit (Str "abc")))))

>>> p ">"
Left (ParsingFailed (Location {name = "", code = ">", offset = 0}))

>>> p "+"
not expected: Plus

>>> p "1+2"
not expected: Plus

>>> p "<1,x>"
Right (Val (LocV 0 (HNF (Tuple [LocV 1 (HNF (Lit (Int 1))),LocV 3 (Var "x")]))))

>>> p "\\z. one"
Left (ParsingFailed (Location {name = "", code = "\\z. one", offset = 7}))

>>> p "\\ one two z. one"
Left (ParsingFailed (Location {name = "", code = "\\ one two z. one", offset = 2}))

>>> p "a;a=b;c"
Right (Seq (Exp (Val (LocV 0 (Var "a")))) (Seq (Eq (LocV 2 (Var "a")) (Val (LocV 4 (Var "b")))) (Val (LocV 6 (Var "c")))))

>>> p "f 2"
Right (App (LocV 0 (Var "f")) (LocV 2 (HNF (Lit (Int 2)))))

>>> p "a | 3 | b"
Right (Loc 6 (Choice (Loc 2 (Choice (Val (LocV 0 (Var "a"))) (Val (LocV 4 (HNF (Lit (Int 3))))))) (Val (LocV 8 (Var "b")))))

>>> p "a | a=b;3 | b"
Right (Loc 2 (Choice (Val (LocV 0 (Var "a"))) (Seq (Eq (LocV 4 (Var "a")) (Val (LocV 6 (Var "b")))) (Loc 10 (Choice (Val (LocV 8 (HNF (Lit (Int 3))))) (Val (LocV 12 (Var "b"))))))))

>>> p "one {3 | 5}"
Right (One (Loc 7 (Choice (Val (LocV 5 (HNF (Lit (Int 3))))) (Val (LocV 9 (HNF (Lit (Int 5))))))))

>>> p "all {3 | 5}"
Right (One (Loc 7 (Choice (Val (LocV 5 (HNF (Lit (Int 3))))) (Val (LocV 9 (HNF (Lit (Int 5))))))))
-}
parse
    :: String
    -- ^ Name of the input (used for error messages)
    -> Text
    -- ^ Source code
    -> Either ParseError Exp
parse name code = do
    tokens <- Lexer.lex name code

    case Earley.fullParses (Earley.parser grammar) tokens of
        ([], Report{..}) -> do
            let offset =
                    case unconsumed of
                        []                -> Offset (Text.length code)
                        locatedToken_ : _ -> Lexer.start locatedToken_

            Left (ParsingFailed (Location{..}))

        (result : _, _) -> do
            return result
