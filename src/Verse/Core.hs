
-- see https://simon.peytonjones.org/assets/pdfs/verse-March23.pdf
{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveLift          #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Verse.Core where
import           Control.Applicative (Applicative((<*>)), (<$>))
import           Control.Monad
import           Control.Monad.Logic
import qualified Control.Monad.State.Strict as State
import           Data.List
import           Data.Text (Text, unpack)
import           GHC.Generics (Generic)
import           Grace.Location (Offset)
import           Grace.Syntax (Syntax(bindings))
import           Options.Applicative.Types (ParseError(UnexpectedError))
import           Prelude hiding (GT, exp, lookup)

{-


>>> e $ App add (All (Choice (i 7) (i 4)))

>>> e $ App (op GT) (All (Choice (i 7) (i 4)))
> all(7 | 4)
{7}

>>> e $ App (op GT) (All (Choice (i 7) (i 17)))
> all(7 | 17)
{}

Scope:[x=:x,y:=y,z=z]

x=<y,3>

<y,3>=<2,z>
y=2;3=z
x=<y,3>;y=2,z=3;x

x y,3
y,3 = 2,z

∃x y z. x = ⟨y, 3⟩; x = ⟨2, z⟩; y

>>> e (App (HNF (Op Add)) (HNF (Tuple [HNF (Lit (Int 11)),HNF (Lit (Int 22))])))
[HNF (Lit (Int 33))]

>>> e (App (HNF (Op Add)) (HNF (Tuple [HNF (Lit (Int 11)),HNF (Lit (Str ""))])))
Wrong operands to Add

>>> e (App (HNF (Op GT)) (HNF (Tuple [HNF (Lit (Int 1)),HNF (Lit (Int 2))])))
[]

>>> evaluate (App (HNF (Op GT)) (HNF (Tuple [HNF (Lit (Int 2)),HNF (Lit (Int 1))])))
[HNF (Lit (Int 2))]

>>> evaluate (Val (Var "x"))
[Var "x"]
-}


-- l = Val . HNF . Tuple . map evaluate

-- Program = one (exp) where exp has no free vars



-- evaluate :: Exp -> [Value]
-- evaluate e =
--      case State.runState (eval e) [] of 
--         (vs,[]) -> vs
--         (_,fs) -> error $ "Unexpected free variables: " ++ show fs

eval :: Exp -> Eval

eval Fail           = return []

eval (Val vv@(Var v)) = do
    ctx <- State.get
    case lookup v ctx of
        Just e  -> eval e
        Nothing -> return [vv] -- error ("Unknown variable: " ++ v) -- fail

eval (Val v)       = return [v]

-- VAL-ELIM
-- v ; e ==> e
eval (Seq (Exp _) e1) = eval e1

-- SUBST BAD!
--eval (Seq (Eq (Var x) (Val v)) e1) = with x (Just (Val v)) e1
eval (Seq (Eq (Var x) (Val v)) e1) = with x (Val v) e1

-- U-LIT
-- k1 = k2 ; e ==> e
eval (Seq (Eq (HNF (Lit l1))  (Val (HNF (Lit l2)))) e1) | l1 == l2 = eval e1

-- U-TUP
-- <a1,..,an> = <b1,..,bn> ; e ==> a1=b1 ; .. ; an=bn ; e
eval (Seq (Eq (HNF (Tuple t1)) (Val (HNF (Tuple t2)))) e1) | length t1 == length t2 = eval $ seqs (zipWith (\v e -> (v,Val e)) t1 t2 ) e1

-- U-FAIL
-- If U_LIT & U_TUP do not match, then FAIL
eval (Seq (Eq _ _) _) = eval Fail

eval (App f a) = do
    case f of
        (HNF (Op p)) -> case p of
            -- APP-ADD
            Add -> case a of
                (HNF (Tuple [HNF (Lit (Int i1)),HNF (Lit (Int i2))])) -> return [int $ i1+i2]
                _ -> error "Non Int operand(s) to Add"
            -- APP-GT, APP-GT-FAIL
            GT -> case a of
                (HNF (Tuple [l1@(HNF (Lit (Int i1))),HNF (Lit (Int i2))])) -> return [l1 | i1 > i2]
                _ -> error "Non Int operand(s) to GT"

        -- APP-BETA
        -- (\x.e) a -> Exists x. x = a; e
        (HNF (Lam x e)) | not (x `freeInVal` a) -> eval $ Exists x (Seq (Eq (Var x) (Val a)) e)
                        -- | otherwise, TODO alpha-conversion

        -- TODO APP-TUP    

        _ -> error "Unimplemented"

-- eval (Exists x e) = with x Nothing e
eval (Exists x e) = with x (Val (Var x)) e

{-
>>> e $ Choice Fail (i 5)
-}
eval (Choice e1 e2) = (++) <$> eval e1 <*> eval e2

eval (One e) = do
    rs <- eval e
    return $ case rs of
        []    -> []
        (x:_) -> [x]

eval (All e) = return . HNF . Tuple <$> eval e

eval (Loc _ e) = eval e

seqs :: [(Value, Exp)] -> Exp -> Exp
seqs [] e = e
seqs ((v,exp):eqs) e = Seq (Eq v exp) $ seqs eqs e

data Exp =
        Fail
        | One Exp
        | All Exp
        | Exists Var Exp
        | Seq ExpOrEq Exp
        | Choice Exp Exp
        | App Value Value
        | Val Value
        | Loc Offset Exp
        deriving (Eq, Generic, Show)

data Value = 
    Var Var 
    | HNF HNF 
    | LocV Offset Value deriving (Eq,Show)

type Var = Text

data ExpOrEq =
    Exp Exp        -- e
    | Eq Value Exp -- v=e
        deriving (Eq,Show)

-- Head Values, Head Normal Form
data HNF = 
      Lit Lit 
    | Op Op
    | Tuple [Value] 
    | Lam Var Exp 
    deriving (Eq,Show)

data Op = GT | Add deriving (Eq,Show)

data Lit = Int Int | Str Text deriving (Show,Eq)

int :: Int -> Value
int = HNF . Lit . Int

-- data V = V Var Int

{-
>> f v = freeIn (Var v) 

>> f "x" $ Val (Var "x")

>>> freeInVal "x" $ Var "y"
False

>>> freeInVal "x" $ HNF (Lam "x" (Val (Var "x")))
-}
freeIn :: Var -> Exp -> Bool
freeIn v = go
  where
    go ex = case ex of
        Val ev -> freeInVal v ev
        Seq (Exp e1) e2 -> go e1 || go e2
        Seq (Eq v1 e1) e2-> go (Val v1) || go e1 || go e2
        Exists x e -> v /= x && go e
        Fail -> False
        Choice e1 e2 -> go e1 || go e2
        App e1 e2 -> freeInVal v e1 || freeInVal v e2
        One e -> go e
        All e -> go e

{-
-}
freeInVal :: Var -> Value -> Bool
freeInVal v ev = case ev of
            Var v'  -> v == v'
            HNF hnf -> case hnf of
                Lit _    -> False
                Op _     -> False
                Tuple vs -> any (freeIn v . Val) vs
                Lam x e  -> v /= x && freeIn v e

-- type Eval = State.State [(Var, Maybe Exp)] [Value]
type Eval = State.State [(Var, Exp)] [Value]

type Scope = State.State [(Var, Exp)]


-- Evaluate an expression in the current context augmented with a bound variable
with :: Var -> Exp -> Exp -> Eval
with v s e = do
    previous <- State.get
    r <- State.withState ((v,s) :) $ eval e
    State.put previous
    return r

-- add v s' e = do
--     ctx <- State.get
--     case lookup v ctx of
--         Just s  -> do
--             unify s s' eval e
--             rs <- State.withState ((v,s) :) $ eval e
--             State.put ctx
--             return rs

        -- Nothing -> error ("Unknown variable: " ++ v) -- fail


