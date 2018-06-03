{-# LANGUAGE
  DeriveFoldable,
  DeriveGeneric,
  DeriveTraversable,
  ViewPatterns,
  FlexibleContexts
#-}

module Lib where

import Control.Monad (MonadPlus(..))
import Control.Monad.Except (MonadError(..))
import Data.Fix (Fix(..))
-- import Data.Sequence (Seq)
-- import Data.Text (Text)
-- import Data.Text as T
import GHC.Generics (Generic)

pureFix :: Applicative m => f (Fix f) -> m (Fix f)
pureFix = pure . Fix

data Term a =
    TmTrue
  | TmFalse
  | TmIf a a a
  | TmZero
  | TmSucc a
  | TmPred a
  | TmIsZero a
  -- | TmLam Text a
  -- | TmApp a a
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

type FixTerm = Fix Term

data Type a =
    TyBool
  | TyNat
  -- | TyFun a a
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

type FixType = Fix Type

isNumericVal :: FixTerm -> Bool
isNumericVal t =
  case unFix t of
    TmZero -> True
    TmSucc t' -> isNumericVal t'
    _ -> False

isBoolVal :: FixTerm -> Bool
isBoolVal t =
  case unFix t of
    TmTrue -> True
    TmFalse -> True
    _ -> False

isVal :: FixTerm -> Bool
isVal = (||) <$> isNumericVal <*> isBoolVal

smallStep :: MonadPlus m => FixTerm -> m FixTerm
smallStep t =
  case unFix t of
    TmIf (unFix -> TmTrue) t2 _ -> return t2
    TmIf (unFix -> TmFalse) _ t3 -> return t3
    TmIf t1 t2 t3 -> do
      t1' <- smallStep t1
      pureFix (TmIf t1' t2 t3)
    TmSucc t1 -> do
      t1' <- smallStep t1
      pureFix (TmSucc t1')
    TmPred (unFix -> TmZero) -> pureFix TmZero
    TmPred (unFix -> TmSucc t1) | isNumericVal t1 -> pure t1
    TmPred t1 -> do
      t1' <- smallStep t1
      pureFix (TmPred t1')
    TmIsZero (unFix -> TmZero) -> pureFix TmTrue
    TmIsZero (unFix -> TmSucc t1) | isNumericVal t1 -> pureFix TmFalse
    TmIsZero t1 -> do
      t1' <- smallStep t1
      pureFix (TmIsZero t1')
    _ -> mzero

evaluate :: FixTerm -> FixTerm
evaluate t =
  case smallStep t of
    Just t' -> evaluate t'
    Nothing -> t

data TypeErr =
    Boom
  | CannotUnify FixType FixType
  deriving (Show, Eq, Generic)

unifyTy :: MonadError TypeErr m => FixType -> FixType -> m FixType
unifyTy t1 t2 =
    case (unFix t1, unFix t2) of
      (TyBool, TyBool) -> pureFix TyBool
      (TyNat, TyNat) -> pureFix TyNat
      _ -> throwError (CannotUnify t1 t2)

checkType :: MonadError TypeErr m => FixType -> FixTerm -> m ()
checkType _ _ = undefined

inferType :: MonadError TypeErr m => FixTerm -> m FixType
inferType t =
  case unFix t of
    TmTrue -> pureFix TyBool
    TmFalse -> pureFix TyBool
    TmIf t1 t2 t3 -> do
      checkType (Fix TyBool) t1
      thenTy <- inferType t2
      elseTy <- inferType t3
      unifyTy thenTy elseTy
    TmZero -> pureFix TyNat
    TmSucc t1 -> do
      checkType (Fix TyNat) t1
      pureFix TyNat
    TmPred t1 -> do
      checkType (Fix TyNat) t1
      pureFix TyNat
    TmIsZero t1 -> do
      checkType (Fix TyNat) t1
      pureFix TyBool
