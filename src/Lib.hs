module Lib where

data Term =
    TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term
  deriving (Show, Eq)

isNumericVal :: Term -> Bool
isNumericVal TmZero = True
isNumericVal (TmSucc t) = isNumericVal t
isNumericVal _ = False

isVal :: Term -> Bool
isVal TmTrue = True
isVal TmFalse = True
isVal t = isNumericVal t

smallStep :: Term -> Maybe Term
smallStep t =
  case t of
    TmIf TmTrue t2 _ -> return t2
    TmIf TmFalse _ t3 -> return t3
    TmIf t1 t2 t3 -> do
      t1' <- smallStep t1
      return (TmIf t1' t2 t3)
    TmSucc t1 -> do
      t1' <- smallStep t1
      return (TmSucc t1')
    TmPred TmZero -> return TmZero
    TmPred (TmSucc t1) | isNumericVal t1 -> return t1
    TmPred t1 -> do
      t1' <- smallStep t1
      return (TmPred t1')
    TmIsZero TmZero -> return TmTrue
    TmIsZero (TmSucc t1) | isNumericVal t1 -> return TmFalse
    TmIsZero t1 -> do
      t1' <- smallStep t1
      return (TmIsZero t1')
    _ -> Nothing

evaluate :: Term -> Term
evaluate t =
  case smallStep t of
    Just t' -> evaluate t'
    Nothing -> t
