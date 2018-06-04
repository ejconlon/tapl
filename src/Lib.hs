{-# LANGUAGE
  DeriveFoldable,
  DeriveGeneric,
  DeriveTraversable,
  FlexibleContexts,
  TemplateHaskell,
  ViewPatterns
#-}

module Lib where

import Bound
import Control.Monad (ap)
-- import Coproduct
import Data.Sequence (Seq)
import Data.Sequence as Seq
import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)
import Data.Functor.Classes
import Eval
import GHC.Generics (Generic)

-- data RawTerm f a =
--     TmTrue
--   | TmFalse
--   | TmZero
--   | TmIsZero !(f a)
--   deriving (Functor, Foldable, Traversable, Generic)

-- data RawTerm a =
--     RTmTrue
--   | RTmFalse
--   | RTmZero
--   | RTmIsZero !a
--   deriving (Functor, Foldable, Traversable, Generic)

-- data BTerm f a =
--     BTmLift !(f a)
--   | BTmVar !a
--   | BTmApp !(Term a) !(Seq (Term a))
--   | BTmLam !(Scope Int Term a)
--   deriving (Functor, Foldable, Traversable, Generic)

class Monad f => Named f where
  lam :: Scope Int f a -> f a
  app :: f a -> Seq (f a) -> f a

  lamN :: Eq a => Seq a -> f a -> f a
  lamN names = lam . abstract (flip Seq.elemIndexL names)

  lam1 :: Eq a => a -> f a -> f a
  lam1 name = lamN (Seq.singleton name)

  app1 :: f a -> f a -> f a
  app1 left right = app left (Seq.singleton right)

-- TERMS

data Term a =
    TmTrue
  | TmFalse
  | TmVar !a
  | TmApp !(Term a) !(Seq (Term a))
  | TmLam !(Scope Int Term a)
  deriving (Functor, Foldable, Traversable, Generic)

deriveEq1 ''Term
deriveOrd1 ''Term
deriveRead1 ''Term
deriveShow1 ''Term

instance Eq a => Eq (Term a) where (==) = eq1
instance Ord a => Ord (Term a) where compare = compare1
instance Read a => Read (Term a) where readsPrec = readsPrec1
instance Show a => Show (Term a) where showsPrec = showsPrec1

instance Applicative Term where
  pure = TmVar
  (<*>) = ap

instance Monad Term where
  return = pure
  t >>= f =
    case t of
      TmTrue -> TmTrue
      TmFalse -> TmFalse
      TmVar a -> f a
      TmApp t1 t2 -> TmApp (t1 >>= f) ((>>= f) <$> t2)
      TmLam s -> TmLam (s >>>= f)

instance Named Term where
  lam = TmLam
  app = TmApp

instance Eval Term where
  holes t =
    case t of
      TmApp body args -> idStep =<< TmApp <$> holes body <*> traverse holes args
      _ -> pure t

  smallHoleStep t =
    case t of
      TmApp body args ->
        case body of
          TmLam s -> Just (instantiate (Seq.index args) s)
          _ -> Nothing
      _ -> Nothing

-- TYPES

data Type a =
    TyBool
  | TyVar !a
  | TyApp !(Type a) !(Seq (Type a))
  | TyLam !(Scope Int Type a)
  deriving (Functor, Foldable, Traversable, Generic)

deriveEq1 ''Type
deriveOrd1 ''Type
deriveRead1 ''Type
deriveShow1 ''Type

instance Eq a => Eq (Type a) where (==) = eq1
instance Ord a => Ord (Type a) where compare = compare1
instance Read a => Read (Type a) where readsPrec = readsPrec1
instance Show a => Show (Type a) where showsPrec = showsPrec1

instance Applicative Type where
  pure = TyVar
  (<*>) = ap

instance Monad Type where
  return = pure
  t >>= f =
    case t of
      TyBool -> TyBool
      TyVar a -> f a
      TyApp t1 t2 -> TyApp (t1 >>= f) ((>>= f) <$> t2)
      TyLam s -> TyLam (s >>>= f)

-- instance Named Type where
--   lam = TyLam
--   app = TyApp
