{-# options_ghc -Wno-unused-foralls #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveFoldable            #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveTraversable         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExtendedDefaultRules      #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedRecordDot       #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

module QQ.Hell where

import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Bifunctor
import           Data.Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Map.Strict (Map)
import           Data.Proxy
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Traversable
import           Data.Void
import           GHC.TypeLits
import           GHC.Types
import qualified Language.Haskell.Exts as HSE
import           Language.Haskell.TH (Q)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Type.Reflection as Type
import           Type.Reflection
    ( SomeTypeRep(..)
    , TypeRep
    , pattern TypeRep
    , someTypeRep
    , typeRep
    , typeRepKind
    )


--------------------------------------------------------------------------------
-- Typed AST support
--
-- We define a well-typed, well-indexed GADT AST which can be evaluated directly.

data Term g t where
  Var :: Var g t -> Term g t
  Lam :: Term (g, a) b -> Term g (a -> b)
  App :: Term g (s -> t) -> Term g s -> Term g t
  Lit :: a -> Term g a

{-
Environment structure: 
(((..,valSS),valS),valZ)

val can be anything, the ZVar function will extract what we want from it.

>>> lookp (ZVar id) (((),("fld1","fld2")),"a")
"a"

>>> lookp (ZVar snd) ((),("fld1","fld2"))
"fld2"

>>> lookp (SVar (ZVar id)) (((),("fld1","fld2")),"a")
("fld1","fld2")

>>> lookp (SVar (ZVar snd)) (((),("fld1","fld2")),"a")
"fld2"

A Var is ~~ env[index].f 
-}
data Var g t where
  ZVar :: (t -> a) -> Var (h, t) a
  SVar :: Var h t -> Var (h, s) t

--------------------------------------------------------------------------------
-- Evaluator
--

{-
>>> eval ((),"abc") $ Var (ZVar head)
'a'

>>> eval (((),2),1) $ Var (ZVar (+1))
2

>>> eval () $ App (Lit reverse) (Lit "abc")
"cba"

>>> eval () $ App (App (Lit (+)) (Lit 1)) (Lit 2)
3

>>> eval (((),2),9) $ App (App (Lit (+)) v0) v1
11


>>> eval (((),2),9) $ App (App (Lit (+)) v0) v1
11

>>> eval () $ Lit "abc"
"abc"

>>> eval (((),(+1)),9) (App v1 v0)
10

>>> eval (((),(+)),9) (App (App v1 v0) v0)
18

>>> e11
18
-}
v1 = Var (SVar (ZVar id))

v0 = Var (ZVar id)

e11 = eval (((),(+)),9) (App (App v1 v0) v0)

-- This is the entire evaluator. Type-safe and total.
eval :: env -> Term env t -> t
eval env (Var v) = lookp v env
eval env (Lam e) = \x -> eval (env, x) e
eval env (App e1 e2) = let f = eval env e1 in f (eval env e2)
eval _env (Lit a) = a


-- Type-safe, total lookup. The final @slot@ determines which slot of
-- a given tuple to pick out.
lookp :: Var env t -> env -> t
lookp (ZVar slot) (_, x) = slot x
lookp (SVar v) (env, _) = lookp v env

--------------------------------------------------------------------------------
-- The "untyped" AST
--
-- This is the AST that is not interpreted, and is just
-- type-checked. The HSE AST is desugared into this one.

type Loc = HSE.SrcSpanInfo
noPos = HSE.noSrcSpan
-- type Loc = ()
ulam x = ULam noPos () (Singleton x) Nothing

uvar = UVar noPos ()

-- idT :: UTerm ()
-- idT = ulam "x" (uvar "x")

data UTerm t
  = UVar Loc t String
  | ULam Loc t Binding (Maybe SomeStarType) (UTerm t)
  | UApp Loc t (UTerm t) (UTerm t)

  -- IRep below: The variables are poly types, they aren't metavars,
  -- and need to be instantiated.
--   | UForall Loc t [SomeTypeRep] Forall [TH.Uniq] (IRep TH.Uniq) [t]
  deriving (Traversable, Functor, Foldable,Show)

typeOf :: UTerm t -> t
typeOf = \case
  UVar _ t _ -> t
  ULam _ t _ _ _ -> t
  UApp _ t _ _ -> t
--   UForall _ t _ _ _ _ _ -> t

data Binding = Singleton String | Tuple [String] deriving Show

data SomeStarType = forall (a :: Type). SomeStarType (TypeRep a)
deriving instance Show SomeStarType
instance Eq SomeStarType where
  SomeStarType x == SomeStarType y = Type.SomeTypeRep x == Type.SomeTypeRep y

pattern StarTypeRep t <- (toStarType -> Just (SomeStarType t)) where
  StarTypeRep t = SomeTypeRep t

toStarType :: SomeTypeRep -> Maybe SomeStarType
toStarType (SomeTypeRep t) = do
  Type.HRefl <- Type.eqTypeRep (typeRepKind t) (typeRep @Type)
  pure $ SomeStarType t

--------------------------------------------------------------------------------
-- The type checker

data Typed (thing :: Type -> Type) = forall ty. Typed (TypeRep (ty :: Type)) (thing ty)

data TypeCheckError
  = NotInScope String
  | TupleTypeMismatch
  | TypeCheckMismatch
  | TupleTypeTooBig
  | TypeOfApplicandIsNotFunction
  | LambdaIsNotAFunBug
  | InferredCheckedDisagreeBug
  | LambdaMustBeStarBug
  deriving (Show)

typed :: Type.Typeable a => a -> Typed (Term g)
typed l = Typed (Type.typeOf l) (Lit l)

-- The type environment and lookup
data TyEnv g where
  Nil :: TyEnv g
  Cons :: Binding -> TypeRep (t :: Type) -> TyEnv h -> TyEnv (h, t)



{-
>> check

>>> Type.someTypeRepTyCon $ someTypeRep (Proxy :: Proxy [Int])
[]

>>> SomeTypeRep $ Type.typeOf $ True
Bool

>>> e
Left (ElabError (VariableNotInScope "a"))
-}

e = inferExp mempty (UVar HSE.noSrcSpan () "a")

e1 = inferExp mempty (UVar HSE.noSrcSpan () "a")

-- The top-level checker used by the main function.
check :: (UTerm SomeTypeRep) -> TyEnv () -> Either TypeCheckError (Typed (Term ()))
check = tc

-- Type check a term given an environment of names.
tc :: (UTerm SomeTypeRep) -> TyEnv g -> Either TypeCheckError (Typed (Term g))
tc (UVar _ _ v) env = do
  Typed ty v' <- lookupVar v env
  pure $ Typed ty (Var v')
tc (ULam _ (StarTypeRep lam_ty) s _ body) env =
  case lam_ty of
    Type.Fun bndr_ty' _ |
      Just Type.HRefl <- Type.eqTypeRep (typeRepKind bndr_ty') (typeRep @Type) ->
      case tc body (Cons s bndr_ty' env) of
        Left e -> Left e
        Right (Typed body_ty' body') ->
          let checked_ty = Type.Fun bndr_ty' body_ty'
          in
           case Type.eqTypeRep checked_ty lam_ty of
             Just Type.HRefl -> Right $ Typed lam_ty (Lam body')
             Nothing -> Left InferredCheckedDisagreeBug
    _ -> Left LambdaIsNotAFunBug
tc (ULam _ (SomeTypeRep{}) _ _ _) _ =
  Left LambdaMustBeStarBug
tc (UApp _ _ e1 e2) env =
  case tc e1 env of
    Left e -> Left e
    Right (Typed (Type.Fun bndr_ty body_ty) e1') ->
      case tc e2 env of
        Left e -> Left e
        Right (Typed arg_ty e2') ->
          case Type.eqTypeRep arg_ty bndr_ty of
            Nothing ->
              -- error $ "Type error: " ++ show arg_ty ++ " vs " ++ show bndr_ty
              Left TypeCheckMismatch
            Just (Type.HRefl) ->
             let kind = typeRepKind body_ty
             in
             case Type.eqTypeRep kind (typeRep @Type) of
               Just Type.HRefl -> Right $ Typed body_ty (App e1' e2')
               _ -> Left TypeCheckMismatch
    Right{} -> Left TypeOfApplicandIsNotFunction
-- Polytyped terms, must be, syntactically, fully-saturated
-- tc (UForall _ _ _ fall _ _ reps0) _env = go reps0 fall where
--   go :: [SomeTypeRep] -> Forall -> Either TypeCheckError (Typed (Term g))
--   go [] (Final typed') = pure typed'
--   go (StarTypeRep rep:reps) (NoClass f) = go reps (f rep)
--   go (SomeTypeRep rep:reps) (ListOf f)
--     | Just Type.HRefl <- Type.eqTypeRep (typeRepKind rep) (typeRep @List) = go reps (f rep)
--   go (SomeTypeRep rep:reps) (SymbolOf f)
--     | Just Type.HRefl <- Type.eqTypeRep (typeRepKind rep) (typeRep @Symbol) = go reps (f rep)
--   go (StarTypeRep rep:reps) (OrdEqShow f) =
--     if
--         | Just Type.HRefl <- Type.eqTypeRep rep (typeRep @Int) -> go reps (f rep)
--         | Just Type.HRefl <- Type.eqTypeRep rep (typeRep @Double) -> go reps (f rep)
--         | Just Type.HRefl <- Type.eqTypeRep rep (typeRep @Bool) -> go reps (f rep)
--         | Just Type.HRefl <- Type.eqTypeRep rep (typeRep @Char) -> go reps (f rep)
--         | Just Type.HRefl <- Type.eqTypeRep rep (typeRep @Text) -> go reps (f rep)
--         | Just Type.HRefl <- Type.eqTypeRep rep (typeRep @ByteString) -> go reps (f rep)
--         | Just Type.HRefl <- Type.eqTypeRep rep (typeRep @ExitCode) -> go reps (f rep)
--         | otherwise -> error $ "type doesn't have enough instances " ++ show rep
--   go (SomeTypeRep rep:reps) (Monadic f) =
--     if
--         | Just Type.HRefl <- Type.eqTypeRep rep (typeRep @IO) -> go reps (f rep)
--         | Just Type.HRefl <- Type.eqTypeRep rep (typeRep @Maybe) -> go reps (f rep)
--         | Just Type.HRefl <- Type.eqTypeRep rep (typeRep @[]) -> go reps (f rep)
--         | Type.App either' _ <- rep,
--           Just Type.HRefl <- Type.eqTypeRep either' (typeRep @Either) -> go reps (f rep)
--         | otherwise -> error $ "type doesn't have enough instances " ++ show rep
--   go reps (GetOf k0 a0 t0 r0 f) =
--           case makeAccessor k0 r0 a0 t0 of
--             Just accessor -> go reps (f accessor)
--             Nothing -> error $ "missing field for field access"
--   go reps (SetOf k0 a0 t0 r0 f) =
--           case makeSetter k0 r0 a0 t0 of
--             Just accessor -> go reps (f accessor)
--             Nothing -> error $ "missing field for field set"
--   go reps (ModifyOf k0 a0 t0 r0 f) =
--           case makeModify k0 r0 a0 t0 of
--             Just accessor -> go reps (f accessor)
--             Nothing -> error $ "missing field for field modify"
--   go tys r = error $ "forall type arguments mismatch: " ++ show tys ++ " for " ++ showR r
--     where showR = \case
--              NoClass{} -> "NoClass"
--              SymbolOf{} -> "SymbolOf"
--              ListOf{} -> "ListOf"
--              OrdEqShow{} -> "OrdEqShow"
--              Monadic{} -> "Monadic"
--              GetOf{} -> "GetOf"
--              SetOf{} -> "SetOf"
--              ModifyOf{} -> "ModifyOf"
--              Final{} -> "Final"

-- Make a well-typed literal - e.g. @lit Text.length@ - which can be
-- embedded in the untyped AST.
lookupVar :: String -> TyEnv g -> Either TypeCheckError (Typed (Var g))
lookupVar str Nil = Left $ NotInScope str
lookupVar v (Cons (Singleton s) ty e)
  | v == s = pure $ Typed ty (ZVar id)
  | otherwise = do
    Typed ty' v' <- lookupVar v e
    pure $ Typed ty' (SVar v')
lookupVar v (Cons (Tuple ss) ty e)
  | Just i <- lookup v $ zip ss [0 :: Int ..] =
    case ty of
      Type.App (Type.App tup x) y
       | Just Type.HRefl <- Type.eqTypeRep tup (typeRep @(,)) ->
          case i of
            0 -> pure $ Typed x $ ZVar \(a,_) -> a
            1 -> pure $ Typed y $ ZVar \(_,b) -> b
            _ -> Left TupleTypeMismatch
      Type.App (Type.App (Type.App tup x) y) z
       | Just Type.HRefl <- Type.eqTypeRep tup (typeRep @(,,)) ->
          case i of
            0 -> pure $ Typed x $ ZVar \(a,_,_) -> a
            1 -> pure $ Typed y $ ZVar \(_,b,_) -> b
            2 -> pure $ Typed z $ ZVar \(_,_,c) -> c
            _ -> Left TupleTypeMismatch
      Type.App (Type.App (Type.App (Type.App tup x) y) z) z'
       | Just Type.HRefl <- Type.eqTypeRep tup (typeRep @(,,,)) ->
          case i of
            0 -> pure $ Typed x $ ZVar \(a,_,_,_) -> a
            1 -> pure $ Typed y $ ZVar \(_,b,_,_) -> b
            2 -> pure $ Typed z $ ZVar \(_,_,c,_) -> c
            3 -> pure $ Typed z' $ ZVar \(_,_,_,d) -> d
            _ -> Left TupleTypeMismatch
      _ -> Left TupleTypeTooBig
  | otherwise = do
     Typed ty' v' <- lookupVar v e
     pure $ Typed ty' (SVar v')

--------------------------------------------------------------------------------
-- Infer

data InferError =
  UnifyError UnifyError
  | ZonkError ZonkError
  | ElabError ElaborateError
  deriving Show

-- | Note: All types in the input are free of metavars. There is an
-- intermediate phase in which there are metavars, but then they're
-- all eliminated. By the type system, the output contains only
-- determinate types.
inferExp ::
  Map String (UTerm SomeTypeRep) ->
  UTerm () ->
  Either InferError (UTerm SomeTypeRep)
inferExp _ uterm =
  case elaborate uterm of
    Left elabError ->  Left $ ElabError elabError
    Right (iterm, equalities) ->
      case unify equalities of
        Left unifyError -> Left $ UnifyError unifyError
        Right subs ->
          case traverse (zonkToStarType subs) iterm of
            Left zonkError -> Left $ ZonkError $ zonkError
            Right sterm -> pure sterm

-- | Zonk a type and then convert it to a type: t :: *
zonkToStarType :: Map IMetaVar (IRep IMetaVar) -> IRep IMetaVar -> Either ZonkError SomeTypeRep
zonkToStarType subs irep = do
  zonked <- zonk (substitute subs irep)
  toSomeTypeRep zonked



--------------------------------------------------------------------------------
-- Inference type representation

data IRep v
  = IVar v
  | IApp (IRep v) (IRep v)
  | IFun (IRep v) (IRep v)
  | ICon SomeTypeRep
  deriving (Functor, Traversable, Foldable, Eq, Ord, Show)

data ZonkError
 = ZonkKindError
 | AmbiguousMetavar
 deriving (Show)

-- | A complete implementation of conversion from the inferer's type
-- rep to some star type, ready for the type checker.
toSomeTypeRep :: IRep Void -> Either ZonkError SomeTypeRep
toSomeTypeRep t = do
  go t

  where
  go :: IRep Void -> Either ZonkError SomeTypeRep
  go = \case
    IVar v -> pure (absurd v)
    ICon someTypeRep -> pure someTypeRep
    IFun a b -> do
      a' <- go a
      b' <- go b
      case (a', b') of
        (StarTypeRep aRep, StarTypeRep bRep) ->
          pure $ StarTypeRep (Type.Fun aRep bRep)
        _ -> Left ZonkKindError
    IApp f a -> do
      f' <- go f
      a' <- go a
      case applyTypes f' a' of
        Just someTypeRep -> pure someTypeRep
        _ -> Left ZonkKindError

-- | Convert from a type-indexed type to an untyped type.
fromSomeStarType :: forall void. SomeStarType -> IRep void
fromSomeStarType (SomeStarType r) = fromSomeType (SomeTypeRep r)

fromSomeType :: forall void. SomeTypeRep -> IRep void
fromSomeType (SomeTypeRep r) = go r where
  go :: forall a. TypeRep a -> IRep void
  go = \case
    Type.Fun a b -> IFun (go a) (go b)
    Type.App a b -> IApp (go a) (go b)
    rep@Type.Con{} -> ICon (SomeTypeRep rep)

--------------------------------------------------------------------------------
-- Inference elaboration phase

newtype IMetaVar = IMetaVar0 Int
  deriving (Ord, Eq, Show)

data Elaborate = Elaborate {
  counter :: Int,
  equalities :: Set (Equality (IRep IMetaVar))
  }

data Equality a = Equality HSE.SrcSpanInfo a a
  deriving (Show, Functor)

-- Equality/ordering that is symmetric.
instance (Ord a) => Eq (Equality a) where
  Equality _ a b == Equality _ c d = Set.fromList [a,b] == Set.fromList [c,d]
instance (Ord a) => Ord (Equality a) where
  Equality _ a b `compare` Equality _ c d = Set.fromList [a,b] `compare` Set.fromList [c,d]

data ElaborateError = UnsupportedTupleSize | BadInstantiationBug | VariableNotInScope String
  deriving (Show)

-- | Elaboration phase.
--
-- Note: The input term contains no metavars. There are just some
-- UForalls, which have poly types, and those are instantiated into
-- metavars.
--
-- Output type /does/ contain meta vars.
elaborate :: UTerm () -> Either ElaborateError (UTerm (IRep IMetaVar), Set (Equality (IRep IMetaVar)))
elaborate = fmap getEqualities . flip runStateT empty . flip runReaderT mempty . go where
  empty = Elaborate{counter=0,equalities=mempty}
  getEqualities (term, Elaborate{equalities}) = (term, equalities)
  go :: UTerm () -> ReaderT (Map String (IRep IMetaVar)) (StateT Elaborate (Either ElaborateError)) (UTerm (IRep IMetaVar))
  go = \case
    UVar l () string -> do
      env <- ask
      ty <- case Map.lookup string env of
             Just typ -> pure typ
             Nothing -> lift $ lift $ Left $ VariableNotInScope string
      pure $ UVar l ty string
    UApp l () f x -> do
      f' <- go f
      x' <- go x
      b <- fmap IVar freshIMetaVar
      equal l (typeOf f') (IFun (typeOf x') b)
      pure $ UApp l b f' x'
    ULam l () binding mstarType body -> do
      a <- case mstarType of
        Just ty -> pure $ fromSomeStarType ty
        Nothing -> fmap IVar freshIMetaVar
      vars <- lift $ bindingVars l a binding
      body' <- local (Map.union vars) $ go body
      let ty = IFun a (typeOf body')
      pure $ ULam l ty binding mstarType body'
    -- UForall l () types forall' uniqs polyRep _ -> do
    --   -- Generate variables for each unique.
    --   vars <- for uniqs \uniq -> do
    --     v <- freshIMetaVar
    --     pure (uniq, v)
    --   -- Fill in the polyRep with the metavars.
    --   monoType <- for polyRep \uniq ->
    --     case List.lookup uniq vars of
    --       Nothing -> lift $ lift $ Left $ BadInstantiationBug
    --       Just var -> pure var
    --   -- Order of types is position-dependent, apply the ones we have.
    --   for_ (zip vars types) \((_uniq, var), someTypeRep) ->
    --     equal l (fromSomeType someTypeRep) (IVar var)
    --   -- Done!
    --   pure $ UForall l monoType types forall' uniqs polyRep (map (IVar . snd) vars)

bindingVars :: HSE.SrcSpanInfo -> IRep IMetaVar -> Binding -> StateT Elaborate (Either ElaborateError) (Map String (IRep IMetaVar))
bindingVars _ irep (Singleton name) = pure $ Map.singleton name irep
bindingVars l tupleVar (Tuple names) = do
  varsTypes <- for names \name -> fmap (name, ) (fmap IVar freshIMetaVar)
  -- it's a left-fold:
  -- IApp (IApp (ICon (,)) x) y
  cons <- makeCons
  equal l tupleVar $ foldl IApp (ICon cons) (map snd varsTypes)
  pure $ Map.fromList varsTypes

  where makeCons = case length names of
         2 -> pure $ SomeTypeRep (typeRep @(,))
         3 -> pure $ SomeTypeRep (typeRep @(,,))
         4 -> pure $ SomeTypeRep (typeRep @(,,,))
         _ -> lift $ Left $ UnsupportedTupleSize

equal :: MonadState Elaborate m => HSE.SrcSpanInfo -> IRep IMetaVar -> IRep IMetaVar -> m ()
equal l x y = modify \elaborate' -> elaborate' { equalities = equalities elaborate' <> Set.singleton (Equality l x y) }

freshIMetaVar :: MonadState Elaborate m => m IMetaVar
freshIMetaVar = do
  Elaborate{counter} <- get
  modify \elaborate' -> elaborate' { counter = counter + 1 }
  pure $ IMetaVar0 counter

--------------------------------------------------------------------------------
-- Unification

data UnifyError =
    OccursCheck
  | TypeMismatch HSE.SrcSpanInfo (IRep IMetaVar) (IRep IMetaVar)
  deriving (Show)

-- | Unification of equality constraints, a ~ b, to substitutions.
unify :: Set (Equality (IRep IMetaVar)) -> Either UnifyError (Map IMetaVar (IRep IMetaVar))
unify = foldM update mempty where
  update existing equality =
    fmap (`extends` existing)
         (examine (fmap (substitute existing) equality))
  examine (Equality l a b)
   | a == b = pure mempty
   | IVar ivar <- a = bindMetaVar ivar b
   | IVar ivar <- b = bindMetaVar ivar a
   | IFun a1 b1 <- a,
     IFun a2 b2 <- b =
       unify (Set.fromList [Equality l a1 a2, Equality l b1 b2])
   | IApp a1 b1 <- a,
     IApp a2 b2 <- b =
       unify (Set.fromList [Equality l a1 a2, Equality l b1 b2])
   | ICon x <- a, ICon y <- b =
      if x == y then pure mempty
                else Left $ TypeMismatch l a b
   | otherwise = Left $ TypeMismatch l a b

-- | Apply new substitutions to the old ones, and expand the set to old+new.
extends :: Map IMetaVar (IRep IMetaVar) -> Map IMetaVar (IRep IMetaVar) -> Map IMetaVar (IRep IMetaVar)
extends new old = fmap (substitute new) old <> new

-- | Apply any substitutions to the type, where there are metavars.
substitute :: Map IMetaVar (IRep IMetaVar) -> IRep IMetaVar -> IRep IMetaVar
substitute subs = go where
  go = \case
    IVar v -> case Map.lookup v subs of
      Nothing -> IVar v
      Just ty -> ty
    ICon c -> ICon c
    IFun a b -> IFun (go a) (go b)
    IApp a b -> IApp (go a) (go b)

-- | Do an occurrs check, if all good, return a binding.
bindMetaVar :: IMetaVar -> IRep IMetaVar
            -> Either UnifyError (Map IMetaVar (IRep IMetaVar))
bindMetaVar var typ
  | occurs var typ = Left OccursCheck
  | otherwise = pure $ Map.singleton var typ

-- | Occurs check.
occurs :: IMetaVar -> IRep IMetaVar -> Bool
occurs ivar = any (==ivar)

-- | Remove any metavars from the type.
--
-- <https://stackoverflow.com/questions/31889048/what-does-the-ghc-source-mean-by-zonk>
zonk :: IRep IMetaVar -> Either ZonkError (IRep Void)
zonk = \case
  IVar {} -> Left AmbiguousMetavar
  ICon c -> pure $ ICon c
  IFun a b -> IFun <$> zonk a <*> zonk b
  IApp a b -> IApp <$> zonk a <*> zonk b

-- | Apply a type `f' with an argument `x', if it is a type function,
-- and the input is the right kind.
applyTypes :: SomeTypeRep -> SomeTypeRep -> Maybe SomeTypeRep
applyTypes (SomeTypeRep f) (SomeTypeRep x) =
  case Type.typeRepKind f of
    Type.App (Type.App (-->) a) _b
      | Just Type.HRefl <- Type.eqTypeRep (-->) (TypeRep @(->)) ->
      case Type.eqTypeRep (Type.typeRepKind x) a of
        Just Type.HRefl ->
          Just $ SomeTypeRep $ Type.App f x
        _ -> Nothing
    _ -> Nothing
