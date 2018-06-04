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
-- import Data.Sequence (Seq)
-- import Data.Text (Text)
-- import Data.Text as T
import Data.Functor.Classes
import GHC.Generics (Generic)
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

-- data RawTerm f a =
--   TmTrue
-- | TmFalse
-- | TmZero
-- | TmIsZero !(f a)
-- deriving (Functor, Foldable, Traversable, Generic)

data Term a =
    TmTrue
  | TmFalse
  | TmPure !a
  | TmApp !(Term a) !(Term a)
  | TmLam !(Scope () Term a)
  deriving (Functor, Foldable, Traversable, Generic)

lam :: Eq a => a -> Term a -> Term a
lam name body = TmLam (abstract1 name body)

deriveEq1 ''Term
deriveOrd1 ''Term
deriveShow1 ''Term

instance Eq a => Eq (Term a) where (==) = eq1
instance Ord a => Ord (Term a) where compare = compare1
instance Show a => Show (Term a) where showsPrec = showsPrec1

instance Applicative Term where
  pure = TmPure
  (<*>) = ap

instance Monad Term where
  return = pure
  t >>= f =
    case t of
      TmTrue -> TmTrue
      TmFalse -> TmFalse
      TmPure a -> f a
      TmApp t1 t2 -> TmApp (t1 >>= f) (t2 >>= f)
      TmLam s -> TmLam (s >>>= f)

data Step s a =
    Done !a
  | Running !s !(s -> Step s a)
  deriving (Functor)

idStep :: s -> Step s s
idStep = flip Running Done

runStepId :: Step s a -> a
runStepId st =
  case st of
    Done a -> a
    Running s f -> runStepId (f s)

smallStepMaybe :: Step s a -> (s -> Maybe s) -> Maybe a
smallStepMaybe st f = go st
  where
    go st' =
      case st' of
        Done a -> Nothing
        Running s g -> do
          s' <- f s
          pure (runStepId (g s'))

starStepMaybe :: Step s a -> (s -> Maybe s) -> Maybe a
starStepMaybe st f = goFirst st
  where
    goFirst st' =
      case st' of
        Done _ -> Nothing
        Running s g -> do
          s' <- f s
          goRest (g s')
    goRest st' =
      case st' of
        Done a -> Just a
        Running s g ->
          case f s of
            Nothing -> Just (runStepId st')
            Just s' -> goRest (g s')

instance Applicative (Step s) where
  pure = Done
  (<*>) = ap

instance Monad (Step s) where
  return = pure
  Done a >>= f = f a
  Running s n >>= f = Running s (\s' -> n s' >>= f)

holes :: Term a -> Step (Term a) (Term a)
holes t =
  case t of
    TmApp body arg -> idStep =<< TmApp <$> holes body <*> holes arg
    _ -> pure t

smallHoleStep :: Term a -> Maybe (Term a)
smallHoleStep t =
  case t of
    TmApp body arg ->
      case body of
        TmLam s -> Just (instantiate1 arg s)
        _ -> Nothing
    _ -> Nothing

smallStep :: Term a -> Maybe (Term a)
smallStep t = smallStepMaybe (holes t) smallHoleStep

starStep :: Term a -> Maybe (Term a)
starStep t = starStepMaybe (holes t) smallHoleStep
