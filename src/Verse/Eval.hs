{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Verse.Eval where
import           Control.Applicative
import           Control.Concurrent.STM
import qualified Control.Concurrent.STM.TMVar as TM
import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.Logic
import qualified Control.Monad.State.Class (get)
import qualified Control.Monad.State.Strict as State
import           Data.List
import qualified Data.Text as T
-- import           Grace.Syntax (Syntax(bindings))
-- import           Options.Applicative.Types (ParseError(UnexpectedError))
import Data.Maybe
import Data.Text ()
import Prelude hiding (GT, exp, lookup)
import Verse.Core
import Verse.Parser
import Verse.Pretty

-- Run the evaluation using the state monad and the logict monad

{- $setup
>>> p = error . show . pretty
>>> i = Val . HNF . Lit . Int
>>> s = Val . HNF . Lit . Str
>>> op = Val . HNF . Op
>>> l = Val . HNF . Tuple
>>> add = op Add
>>> e exp = error $ pr exp <> "\n" <> pr (evaluate exp)
-}

-- t :: Exp -> IO ()
-- t exp = putStrLn $ pr exp <> "\n" <> pr (evaluate exp)

-- t :: String -> String
test source out =
    let s = T.pack source
        ee = parse "" s
    in case ee of
        Left err -> unwords ["PARSE ERROR",source,show err]
        Right e -> let
                        p = pr e
                        r = pr $ evaluate e
                    in if source == p
                            then if out == r
                                    then unwords [source," => ",out]
                                    else unwords ["EVAL ERROR",source,"parsed as",p,show e,"returns",r,"=/=",out]
                            else unwords ["PPRINT ERROR",source,"=/=",p]

t = mapM_ (putStrLn .uncurry test) tests

{-
Desugaring
1..3 = 1 | 2 | 3
-}

{-
Tests to write

first :=𝜆p. ∃ab. p = ⟨a, b⟩; a
∃x y. x = ⟨y, 5⟩; 2=first(x); y
==> 2

bi-directional append
append :=𝜆⟨𝑥𝑠, 𝑦𝑠⟩. ( (𝑥𝑠 = ⟨⟩; 𝑦𝑠) (∃x xr. 𝑥𝑠 = ⟨x, xr⟩; ⟨x, append⟨xr, 𝑦𝑠⟩⟩))
single = <1,<>>; ∃𝑧𝑠. append⟨𝑧𝑠, single⟩ =single; zs
-> <>

x=y|2 y=7|8 (x,y) -> (7,7) (8,8) (2,7) (2,8)  

as=[2,5,7] ; for=all{i=1..length(as) ; as[i]+1 } => [3,6,8]

swap (x,y) = (y,x)

swap(3,4) 
=> (4,3)

x:int; y:int ; swap(x,y) = (3,4)
=> x=3, y=4
-}


{-

>>> e $ Exists "x" $ Seq (Eq (Var "x") (Val (HNF (Lit (Int 5))))) (Val (Var "x"))
∃x.x=5;x
{5}


>>> e $ Exists "x" $ Exists "y" $ Exists "z" $ Seq (Eq (Var "x") (Val (HNF $ Tuple [Var "y",int 3]))) $ Seq (Eq (Var "x") (Val (HNF $ Tuple [int 2,Var "z"]))) (Val (Var "x"))
∃x.∃y.∃z.x=<y, 3>;x=<2, z>;x
{<2, z>}
-}

{-
>>> e $ i 1
-}

{-

>>> e $ Choice Fail (i 5)
fail | 5
{5}

>>> e $ Choice (s "abc") (i 5)
abc | 5
{abc, 5}

-}

{-
>>> e $ One $ Fail
one(fail)
{}

>>> e $ One $ (i 11)
one(11)
{11}

>>> e $ One (Choice (i 7) (i 5))
one(7 | 5)
{7}
-}

{-
  >>> e (App (HNF (Op Add)) (HNF (Tuple [HNF (Lit (Int 11)),HNF (Lit (Int 22))])))
-}

{-
>>> e $ Fail
fail

>>> e $ Exists "x" (Seq (Eq (Var "x") Fail) (Val (Var "x")))
∃x.x=fail;x
-}

-- g =  Exists "x" (Seq (Eq (Var "x") Fail) (Val (Var "x")))

type Env = [(Var,Value)]

evaluate :: Exp -> [Value]
evaluate = evaluateWith []

evaluateWith :: Env -> Exp -> [Value]
evaluateWith st e =
     case State.runState (observeAllT $ evalM e) st of
        (vs,[]) -> vs
        (vs,_fs) -> vs -- error $ "Unexpected free variables: " ++ show fs


tests =
    [("fail","")
    ,("fail | 5","5")
    ,("one{fail}","")
    ,("one{1}","1")
    ,("one{1 | \"abc\"}","1")
    ,("all{1 | 2}","<1,2>")
    ,("all{fail}","<>")
    ,("<>","<>")
    ,("<11,22,33> 0","11")
    ,("<11,22,33> 2","33")
    ,("<11,22,33> 3","")
    ,("<11,22,33> -1","")
    -- ,("v","")     -- Execption
    -- ,("∃x.x","x") -- Loops !
    ,("add <3,4>","7")
    ,("gt <3,2>","3")    
    ,("gt <3,3>","")    
    -- ,("∃t.t=<11,22,33>;t 0","11")
    -- ,("∃x.x=5;x","5")
    -- ,("∃x.x=fail;33","FAILS FOR THE WRONG REASON")
    -- ,("∃x.x","x")
    -- ,("∃x.∃y.∃z.x=<y,3>;x=<2,z>;y","")
    -- ∃x. (x =3; x + 1) (x =4; x + 4)
    -- first :=𝜆p. ∃ab. p = ⟨a, b⟩; a  ++ ∃x y. x = ⟨y, 5⟩; 2=first(x); y
    ]

evalM :: Exp -> EvalM

evalM Fail           = fail "fail"

evalM (One e) = once (evalM e)

evalM (All e) = HNF . Tuple <$> State.lift (observeAllT $ evalM e)

evalM (Exists x e) = withM x (Var x) e

-- VAL-ELIM
-- v ; e ==> e
evalM (Seq (Exp _) e1) = evalM e1

-- SUBST BAD!
--eval (Seq (Eq (Var x) (Val v)) e1) = with x (Just (Val v)) e1
-- evalM (Seq (Eq (Var x) v@(Val _)) e1) = withM x v e1

-- U-LIT
-- k1 = k2 ; e ==> e
-- evalM (Seq (Eq (HNF (Lit l1))  (Val (HNF (Lit l2)))) e1) | l1 == l2 = evalM e1

-- U-TUP
-- <a1,..,an> = <b1,..,bn> ; e ==> a1=b1 ; .. ; an=bn ; e
-- evalM (Seq (Eq (HNF (Tuple t1)) (Val (HNF (Tuple t2)))) e1) | length t1 == length t2 = evalM $ seqs (zipWith (\v e -> (v,Val e)) t1 t2 ) e1

-- U-FAIL
-- If U_LIT & U_TUP do not match, then FAIL
-- evalM (Seq (Eq e1 e2) _) = error $ unwords ["not equal", pr e1, pr e2]

evalM (Seq (Eq e1 e2) e) = do
    e1' <- evalV e1
    e2' <- evalM e2
    unify e1' e2'
    evalM e

evalM (Choice e1 e2) = evalM e1 <|> evalM e2

evalM (App f v) = do
    ef <- evalV f
    ev <- evalV v
    case ef of
        
        (HNF (Op p)) -> case p of
            -- APP-ADD
            Add -> case ev of
                (HNF (Tuple [HNF (Lit (Int i1)),HNF (Lit (Int i2))])) -> return (int $ i1+i2)
                _ -> error $ "Non Int operand(s) to Add: " ++ show ev
            -- APP-GT, APP-GT-FAIL
            GT -> case ev of
                (HNF (Tuple [l1@(HNF (Lit (Int i1))),HNF (Lit (Int i2))])) -> if i1 > i2 then return l1 else fail "GT"
                _ -> error "Non Int operand(s) to GT"

        (HNF (Tuple t)) -> case ev of
            HNF (Lit (Int i)) ->
                if i<0 || i >= length t then fail "index out of bounds"
                else return $ t !! i
            _ -> error "Non Int operand to tuple application"

        _ -> error "unimplemented app"

evalM (Val v) = evalV v

evalM (Loc _ e) = evalM e

-- evalM o = error ("unimplemented op: " ++ show o)

-- Intepreted as failure in LogicT
-- evalM o = throwError (strMsg $ "unimplemented: " ++ show o)
-- evalM o = fail $ "unimplemented: " ++ show o

lookupVar :: T.Text -> EvalM
lookupVar var = do
    env <- State.lift State.get
    case lookup var env of
        Just val  -> return val -- $ fromMaybe (Var v) maybeValue
        Nothing -> error ("Unknown variable: " ++ T.unpack var)

evalV :: Value -> EvalM
evalV (LocV _ v) = evalV v

evalV (HNF (Tuple vs)) = HNF . Tuple <$> mapM evalV vs

evalV (Var v) = lookupVar v

evalV v       = return v

-- U-LIT

unify :: Value -> Value -> Eval_ ()
unify (HNF (Lit l1))  (HNF (Lit l2)) | l1 /= l2 = fail "U-LIT"

-- U-TUP
unify (HNF (Tuple t1)) (HNF (Tuple t2)) | length t1 == length t2 = 
    mapM_ (uncurry unify) $ zip t1 t2

-- U-FAIL
unify (HNF _) (HNF _) = fail "U-FAIL"

-- HNF-SWAP
unify h@(HNF _) v@(Var _) = unify v h

unify (Var x) v =  do
    xValue <- lookupVar x
    unify xValue v

unify _ _ = error "cannot unify"

type Eval_ = LogicT (State.State [(Var, Value)])

type EvalM = Eval_ Value

-- withM :: Var -> Exp -> Exp -> EvalM
withM :: Var -> Value -> Exp -> LogicT (State.State [(Var, Value)]) Value
withM v s e = do
    ctx <- State.lift State.get
    State.lift $ State.put ((v,s) : ctx )
    r <- evalM e
    State.lift $ State.put ctx
    return r

pr:: Pretty a => a -> String
pr = T.unpack . toText


{-
x1=2
x2=3
y=2
-}

-- ∃xyz.x=⟨y,3⟩; x=⟨2,z⟩; y
s1 = do
    -- ∃xyz
    x <- TM.newEmptyTMVar
    x1 <- TM.newEmptyTMVar
    x2 <- TM.newEmptyTMVar
    y   <- TM.newEmptyTMVar
    z   <- TM.newEmptyTMVar


    -- x=⟨y,3⟩
    -- x=<x1,y1>
    do
        v1 <- TM.takeTMVar x1
        v2 <- TM.takeTMVar x2
        TM.writeTMVar x (v1,v2)

    y >>> x1

    x1 >>> y

    TM.putTMVar x2 3

    -- x=⟨2,z⟩
    TM.putTMVar x1 2 -- 

    z >>> x2

    x2 >>> z

    -- ;y
    TM.takeTMVar y 

-- (>>>) :: TM.TMVar a -> TM.TMVar a -> STM ()
(>>>) :: TMVar a -> TMVar a -> STM ()
v1 >>> v2 = TM.takeTMVar v1 >>= TM.putTMVar v2


-- asInt :: Value -> Integer
-- asInt (HNF (Lit (Int n))) = n
-- asInt v                   = error $ "Not an Int: " ++ show v

