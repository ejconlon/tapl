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
import Data.Functor.Classes
import GHC.Generics (Generic)
import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)

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

data Term a =
    TmTrue
  | TmFalse
  | TmVar !a
  | TmApp !(Term a) !(Seq (Term a))
  | TmLam !(Scope Int Term a)
  deriving (Functor, Foldable, Traversable, Generic)

lam :: Eq a => Seq a -> Term a -> Term a
lam names = TmLam . abstract (flip Seq.elemIndexL names)

lam1 :: Eq a => a -> Term a -> Term a
lam1 name = lam (Seq.singleton name)

app1 :: Term a -> Term a -> Term a
app1 left right = TmApp left (Seq.singleton right)

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

class Eval f where
  holes :: f a -> Step (f a) (f a)
  smallHoleStep :: f a -> Maybe (f a)

smallStep :: Eval f => f a -> Maybe (f a)
smallStep t = smallStepMaybe (holes t) smallHoleStep

starStep :: Eval f => f a -> Maybe (f a)
starStep t = starStepMaybe (holes t) smallHoleStep

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
