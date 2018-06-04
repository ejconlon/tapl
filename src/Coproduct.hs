module Coproduct
  ( Coproduct(..)
  , left
  , right
  , coproduct
  ) where

-- NOTE Can't find this anymore, it's 90% from an old comonad version by ekmett
-- https://hackage.haskell.org/package/comonad-4.0/docs/Data-Functor-Coproduct.html

import Data.Functor.Classes (Eq1(..), Ord1(..))
import Data.Foldable
import Data.Traversable

newtype Coproduct f g a = Coproduct { getCoproduct :: Either (f a) (g a) }
  deriving (Eq, Ord, Read, Show)

left :: f a -> Coproduct f g a
left = Coproduct . Left

right :: g a -> Coproduct f g a
right = Coproduct . Right

coproduct :: (f a -> b) -> (g a -> b) -> Coproduct f g a -> b
coproduct f g = either f g . getCoproduct

instance (Functor f, Functor g) => Functor (Coproduct f g) where
  fmap f = Coproduct . coproduct (Left . fmap f) (Right . fmap f)

instance (Foldable f, Foldable g) => Foldable (Coproduct f g) where
  foldMap f = coproduct (foldMap f) (foldMap f)

instance (Traversable f, Traversable g) => Traversable (Coproduct f g) where
  traverse f = coproduct
    (fmap (Coproduct . Left) . traverse f)
    (fmap (Coproduct . Right) . traverse f)

instance (Eq1 f, Eq1 g) => Eq1 (Coproduct f g) where
  liftEq p c1 c2 =
    case (getCoproduct c1, getCoproduct c2) of
      (Left f1, Left f2) -> liftEq p f1 f2
      (Right g1, Right g2) -> liftEq p g1 g2
      _ -> False

instance (Ord1 f, Ord1 g) => Ord1 (Coproduct f g) where
  liftCompare p c1 c2 =
    case (getCoproduct c1, getCoproduct c2) of
      (Left f1, Left f2) -> liftCompare p f1 f2
      (Left _, Right _) -> LT
      (Right g1, Right g2) -> liftCompare p g1 g2
      (Right _, Left _) -> GT
