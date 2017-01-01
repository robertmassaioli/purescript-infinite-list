-- | This module allows the creation of infinite lists in Purescript where you
-- | only pay the performance cost for each element in the infinite list when
-- | it is extracted.
-- |
-- | Every infinite list requires three different classes of functions:
-- |
-- | * Generators of infinite lists (iterate, repeat)
-- | * Manipulators of infinite lists (map, filter, zip, tail)
-- | * Getters of data from infinite lists (head, take, takeWhile)

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

-- | The datatype that represents an infinite list. The two type arguments are:
-- |
-- | * `a`: The type that will be returned by the getter methods.
-- | * `b`: The base type for this infinite list that is being iterated over.
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

-- | ### Infinite list generators


-- | Generate an infinite list by providing:
-- |
-- |  * a function to iterate from the previous value in the infinite list to the next AND
-- |  * the first element in the infinite list to begin the iteration.
-- |
-- | For example, to generate the list of natural numbers you can do the following:
-- |
-- |     naturals = iterate ((+) 1) 1
-- |
-- | In english, "The natural numbers start at 1 and you can get the next natural
-- | number by adding one to the prior number".
-- |
-- | Running time: `O(1)`
iterate :: forall a. (a -> a) -> a -> InfiniteList a a
iterate iter start = IL id iter start

-- | Generate an infinite list by infinitely repeating a provided non-empty list.
-- |
-- | The list that is provided must be non-empty because it is impossible to repeat
-- | an empty list and end up with a list with something in it.
-- |
-- | To see how this method works see the following example:
-- |
-- |     > IL.take 15 $ repeat (0 :| fromFoldable [2, 1, 3])
-- |     (0 : 2 : 1 : 3 : 0 : 2 : 1 : 3 : 0 : 2 : 1 : 3 : 0 : 2 : 1 : Nil)
-- |
-- |     >
-- |
-- | This is an infinite repeat of the list 0, 2, 1, 3.
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
tail il = (uncons il).tail

uncons :: forall a b. InfiniteList b a -> { head :: a, tail :: InfiniteList b a }
uncons (IL conv iter start) = { head: conv start, tail: IL conv iter (iter start) }

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
    go il = let u = uncons il
            in if keep u.head then u.head `L.Cons` go u.tail else L.Nil

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
