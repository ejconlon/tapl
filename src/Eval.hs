{-# LANGUAGE
  DeriveFunctor
#-}

module Eval where

import Control.Monad (ap)

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
