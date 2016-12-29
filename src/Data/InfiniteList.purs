module Data.InfiniteList
  ( InfiniteList
  , iterate
  , repeat
  , take
  , drop
  , takeWhile
  , dropWhile
  , head
  , tail
  , zip
  ) where

import Data.Array as A
import Data.Either as E
import Data.Filterable as F
import Data.List as L
import Data.NonEmpty as NE
import Data.Function (id)
import Data.Functor (class Functor, map)
import Data.Maybe (Maybe(..), isJust, fromJust)
import Data.Tuple (Tuple(..), snd)
import Partial.Unsafe (unsafePartialBecause)
import Prelude ((-), (+), ($), (<<<), (<=), otherwise)

data InfiniteList b a = IL (b -> a) (b -> b) b

-- Can't implement these classes
-- Eq: can't compare functions
-- Show: can't iterate the whole list...will let other people implement custom show methods
-- Ord: Requires Eq to be possible
-- Semigroup: Would be stupid ... since the lists are infinite xs <> ys => xs because you would never get to the elements of ys
-- Monoid: Infinite lists, by definition, cannot be empty. What would mempty be?
-- Foldable: You can't fold an infinite list
-- Traversable: You can't traverse the entirity of an infinite list
-- Alt: No, for the same reason as semigroup
-- Plus: Depends on Alt

instance infiniteFunctor :: Functor (InfiniteList a) where
  map f (IL conv iter start) = IL (f <<< conv) iter start

{-
class (Functor f) <= Filterable f where
  partitionMap :: forall a l r.
    (a -> Either l r) -> f a -> { left :: f l, right :: f r }

  partition :: forall a.
    (a -> Boolean) -> f a -> { no :: f a, yes :: f a }

  filterMap :: forall a b.
    (a -> Maybe b) -> f a -> f b

  filter :: forall a.
    (a -> Boolean) -> f a -> f a
-}

instance infiniteFilterable :: F.Filterable (InfiniteList a) where
  partitionMap = partitionMapInternal

  partition = partitionInternal

  filterMap = filterMapInternal

  filter = filterInternal

partitionMapInternal :: forall a b l r. (a -> E.Either l r) -> InfiniteList b a -> { left :: InfiniteList b l, right :: InfiniteList b r }
partitionMapInternal f il = {
    left: fromLeft applied,
    right: fromRight applied
  }
  where
    applied = map f il

fromLeft :: forall a b c. InfiniteList b (E.Either a c) -> InfiniteList b a
fromLeft = map (unsafePartialBecause "We have filtered on only the left elements" E.fromLeft) <<< filterInternal E.isLeft

fromRight :: forall a b c. InfiniteList b (E.Either c a) -> InfiniteList b a
fromRight = map (unsafePartialBecause "We have filtered on only the right elements" E.fromRight) <<< filterInternal E.isRight

partitionInternal :: forall a b. (a -> Boolean) -> InfiniteList b a -> { no :: InfiniteList b a, yes :: InfiniteList b a }
partitionInternal p xs = let o = partitionMapInternal (F.eitherBool p) xs
                         in {no: o.left, yes: o.right}

filterMapInternal :: forall a b c. (a -> Maybe c) -> InfiniteList b a -> InfiniteList b c
filterMapInternal f = catMaybes <<< map f

catMaybes :: forall b a. InfiniteList b (Maybe a) -> InfiniteList b a
catMaybes = map (unsafePartialBecause "We have filtered on only the Just elements" fromJust) <<< filterInternal isJust

filterInternal :: forall a b. (a -> Boolean) -> InfiniteList b a -> InfiniteList b a
filterInternal matches (IL conv iter start) = IL conv next nextStart
  where
    nextStart = if matches' start then start else next start

    next x = let step = iter x in
             if matches' step then step else next step

    matches' = matches <<< conv

-- We could possibly implement Apply if the need for it was there

iterate :: forall a. (a -> a) -> a -> InfiniteList a a
iterate iter start = IL id iter start

repeat :: forall a. NE.NonEmpty L.List a -> InfiniteList (Tuple Int a) a
repeat oxs = IL snd iter firstVal
  where
    ref = index 0 (L.toUnfoldable <<< NE.oneOf $ oxs)

    index :: Int -> Array a -> Array (Tuple Int a)
    index n values = case A.uncons values of
      Just {head: x, tail: xs} -> Tuple n x `A.cons` index (n + 1) xs
      Nothing -> []

    iter :: Tuple Int a -> Tuple Int a
    iter (Tuple cIndex _) = case A.index ref (cIndex + 1) of
      Just x -> x
      Nothing -> firstVal

    firstVal = Tuple 0 (NE.head oxs)

head :: forall a b. InfiniteList b a -> a
head (IL conv _ x) = conv x

tail :: forall a b. InfiniteList b a -> InfiniteList b a
tail (IL conv iter current) = IL conv iter (iter current)

take :: forall a b. Int -> InfiniteList b a -> L.List a
take n il | n <= 0    = L.Nil
          | otherwise = head il `L.Cons` take (n - 1) (tail il)

drop :: forall a b. Int -> InfiniteList b a -> InfiniteList b a
drop n il | n <= 0    = il
          | otherwise = drop (n - 1) (tail il)

takeWhile :: forall a b. (a -> Boolean) -> InfiniteList b a -> L.List a
takeWhile keep = go
  where
    go :: InfiniteList b a -> L.List a
    go il = let h = head il
            in if keep h then h `L.Cons` go (tail il) else L.Nil

dropWhile :: forall a b. (a -> Boolean) -> InfiniteList b a -> InfiniteList b a
dropWhile discard = go
  where
    go :: InfiniteList b a -> InfiniteList b a
    go il = if discard (head il) then go (tail il) else il

zip :: forall a b c d. InfiniteList c a -> InfiniteList d b -> InfiniteList (Tuple c d) (Tuple a b)
zip (IL lConv lIter lStart) (IL rConv rIter rStart) = IL conv iter start
  where
    conv (Tuple x y) = Tuple (lConv x) (rConv y)
    iter (Tuple x y) = Tuple (lIter x) (rIter y)
    start = Tuple lStart rStart