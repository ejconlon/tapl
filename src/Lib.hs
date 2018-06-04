{-# LANGUAGE
  DeriveFoldable,
  DeriveGeneric,
  DeriveTraversable,
  FlexibleContexts,
  TemplateHaskell,
  TypeFamilies
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

class Monad f => Named f where
  type Raw f :: * -> *

  lam :: Scope Int f a -> f a
  app :: f a -> Seq (f a) -> f a
  embed :: Raw f (f a) -> f a

  lamN :: Eq a => Seq a -> f a -> f a
  lamN names = lam . abstract (flip Seq.elemIndexL names)

  lam1 :: Eq a => a -> f a -> f a
  lam1 name = lamN (Seq.singleton name)

  app1 :: f a -> f a -> f a
  app1 left right = app left (Seq.singleton right)

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

data Term a =
    TmEmbed !(RawTerm (Term a))
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
      TmEmbed r -> TmEmbed ((>>= f) <$> r)
      TmVar a -> f a
      TmApp t1 t2 -> TmApp (t1 >>= f) ((>>= f) <$> t2)
      TmLam s -> TmLam (s >>>= f)

instance Named Term where
  type Raw Term = RawTerm

  lam = TmLam
  app = TmApp
  embed = TmEmbed

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
    TyEmbed !(RawType (Type a))
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
      TyEmbed r -> TyEmbed ((>>= f) <$> r)
      TyVar a -> f a
      TyApp t1 t2 -> TyApp (t1 >>= f) ((>>= f) <$> t2)
      TyLam s -> TyLam (s >>>= f)

-- instance Named Type where
--   lam = TyLam
--   app = TyApp
