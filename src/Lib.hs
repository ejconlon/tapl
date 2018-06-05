{-# LANGUAGE
  DeriveFoldable,
  DeriveGeneric,
  DeriveTraversable,
  FlexibleContexts,
  GeneralizedNewtypeDeriving,
  TemplateHaskell,
  TypeFamilies
#-}

module Lib where

import Bound
import Bound.Name
import Control.Monad (ap)
import Control.Monad.Except (MonadError(..))
import Control.Monad.Reader (MonadReader(..), ReaderT(..), withReaderT)
import Data.Fix
import Data.Foldable (for_)
import Data.Map.Strict (Map)
import Data.Map.Strict as Map
import Data.Sequence (Seq)
import Data.Sequence as Seq
import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)
import Data.Functor.Classes
import Eval
import GHC.Generics (Generic)

pureFix :: Applicative m => f (Fix f) -> m (Fix f)
pureFix = pure . Fix

-- TYPES

data RawType a =
    RTyBool
  | RTyNat
  deriving (Functor, Foldable, Traversable)

deriveEq1 ''RawType
deriveOrd1 ''RawType
deriveRead1 ''RawType
deriveShow1 ''RawType

instance Eq a => Eq (RawType a) where (==) = eq1
instance Ord a => Ord (RawType a) where compare = compare1
instance Read a => Read (RawType a) where readsPrec = readsPrec1
instance Show a => Show (RawType a) where showsPrec = showsPrec1

data Type a =
    TyBool
  | TyNat
  | TyFun !a !(Seq a)
  deriving (Functor, Foldable, Traversable, Generic)

type FixType = Fix Type

deriveEq1 ''Type
deriveOrd1 ''Type
deriveRead1 ''Type
deriveShow1 ''Type

instance Eq a => Eq (Type a) where (==) = eq1
instance Ord a => Ord (Type a) where compare = compare1
instance Read a => Read (Type a) where readsPrec = readsPrec1
instance Show a => Show (Type a) where showsPrec = showsPrec1

-- TERMS

data RawTerm a =
    RTmTrue
  | RTmFalse
  | RTmZero
  | RTmIsZero !a
  deriving (Functor, Foldable, Traversable, Generic)

deriveEq1 ''RawTerm
deriveOrd1 ''RawTerm
deriveRead1 ''RawTerm
deriveShow1 ''RawTerm

instance Eq a => Eq (RawTerm a) where (==) = eq1
instance Ord a => Ord (RawTerm a) where compare = compare1
instance Read a => Read (RawTerm a) where readsPrec = readsPrec1
instance Show a => Show (RawTerm a) where showsPrec = showsPrec1

-- TODO add info param
data Term n a =
    TmEmbed !(RawTerm (Term n a))
  | TmVar !a
  | TmApp !(Term n a) !(Seq (Term n a))
  | TmLam !(Seq (n, FixType)) !(Scope (Name n Int) (Term n) a)
  deriving (Functor, Foldable, Traversable, Generic)

deriveEq1 ''Term
deriveOrd1 ''Term
deriveRead1 ''Term
deriveShow1 ''Term

instance (Eq n, Eq a) => Eq (Term n a) where (==) = eq1
instance (Ord n, Ord a) => Ord (Term n a) where compare = compare1
instance (Read n, Read a) => Read (Term n a) where readsPrec = readsPrec1
instance (Show n, Show a) => Show (Term n a) where showsPrec = showsPrec1

instance Applicative (Term n) where
  pure = TmVar
  (<*>) = ap

instance Monad (Term n) where
  return = pure
  t >>= f =
    case t of
      TmEmbed r -> TmEmbed ((>>= f) <$> r)
      TmVar a -> f a
      TmApp t1 t2 -> TmApp (t1 >>= f) ((>>= f) <$> t2)
      TmLam tys s -> TmLam tys (s >>>= f)

app :: Term n a -> Seq (Term n a) -> Term n a
app = TmApp

embed :: RawTerm (Term n a) -> Term n a
embed = TmEmbed

lam :: Eq n => Seq (n, FixType) -> Term n n -> Term n n
lam ntys body = TmLam ntys (abstractName (flip Seq.elemIndexL (fst <$> ntys)) body)

lam1 :: Eq n => n -> FixType -> Term n n -> Term n n
lam1 name ty = lam (Seq.singleton (name, ty))

app1 :: Term n a -> Term n a -> Term n a
app1 left right = app left (Seq.singleton right)

nameIndex :: Name n b -> b
nameIndex (Name _ b) = b

instance Eval (Term n) where
  -- TODO add IsZero hole
  holes t =
    case t of
      TmApp body args -> idStep =<< TmApp <$> holes body <*> traverse holes args
      _ -> pure t

  smallHoleStep t =
    case t of
      TmApp body args ->
        case body of
          TmLam _ s -> Just (instantiate (Seq.index args . nameIndex) s)
          _ -> Nothing
      _ -> Nothing

data TypeErr n a =
    ArityMismatch Int Int
  | TypeMismatch FixType FixType
  | MissingBoundType n Int
  | MissingFreeType a
  | AppNotLam
  deriving (Show, Eq, Functor, Foldable, Traversable)

type TypeCtx a = a -> Maybe FixType

newtype CheckM n a x =
  CheckM {
    unCheckM :: ReaderT (TypeCtx a) (Either (TypeErr n a)) x
  } deriving (Functor, Applicative, Monad, MonadReader (TypeCtx a), MonadError (TypeErr n a))

runCheckM :: CheckM n a x -> TypeCtx a -> Either (TypeErr n a) x
runCheckM c = runReaderT (unCheckM c)

bindCtx :: Seq (n, FixType) -> TypeCtx a -> TypeCtx (Var (Name n Int) a)
bindCtx tys ctx var =
  case var of
    F a -> ctx a
    B (Name _ i) -> snd <$> Seq.lookup i tys

unbindErr :: TypeErr n (Var (Name n Int) a) -> TypeErr n a
unbindErr err =
  case err of
    ArityMismatch x y -> ArityMismatch x y
    TypeMismatch x y -> TypeMismatch x y
    MissingBoundType n i -> MissingBoundType n i
    MissingFreeType var ->
      case var of
        B (Name n i) -> MissingBoundType n i
        F a -> MissingFreeType a
    AppNotLam -> AppNotLam

overLeft :: (e -> e') -> Either e a -> Either e' a
overLeft f ei =
  case ei of
    Left e -> Left (f e)
    Right a -> Right a

withTypeCtx :: (TypeCtx a -> TypeCtx b) -> (TypeErr n b -> TypeErr n a) -> CheckM n b x -> CheckM n a x
withTypeCtx f g c = CheckM (ReaderT (\r -> overLeft g (runCheckM c (f r))))

checkType :: Ord a => FixType -> Term n a -> CheckM n a ()
checkType ety tm = do
  aty <- inferType tm
  if ety /= aty
    then throwError (TypeMismatch aty ety)
    else pure ()

inferRawType :: Ord a => RawTerm (Term n a) -> CheckM n a FixType
inferRawType rtm =
  case rtm of
    RTmTrue -> pureFix TyBool
    RTmFalse -> pureFix TyBool
    RTmZero -> pureFix TyNat
    RTmIsZero a -> checkType (Fix TyNat) a *> pureFix TyNat

inferType :: Ord a => Term n a -> CheckM n a FixType
inferType tm =
  case tm of
    TmEmbed r -> inferRawType r
    TmVar a -> do
      res <- ask
      case res a of
        Nothing -> throwError (MissingFreeType a)
        Just ty -> pure ty
    TmApp body args -> do
      case body of
        TmLam tys s -> do
          let expected = Seq.length tys
              actual = Seq.length args
          _ <- if expected /= actual
                then throwError (ArityMismatch expected actual)
                else pure ()
          for_ (Seq.zip tys args) (\((_, ty), arg) -> checkType ty arg)
          inferType body
        _ -> throwError AppNotLam
    TmLam tys s -> do
      let scopedTerm = fromScope s
      bodyTy <- withTypeCtx (bindCtx tys) unbindErr (inferType scopedTerm)
      pureFix (TyFun bodyTy (snd <$> tys))
