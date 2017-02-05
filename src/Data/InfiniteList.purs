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
  , uncons
  , zip
  , zipWith
  , module Exports
  ) where

import Data.Array as A
import Data.Either as E
import Data.Exists (Exists, mkExists, runExists)
import Data.Filterable as F
import Data.NonEmpty as NE
import Data.Filterable (filter) as Exports
import Data.Function (id)
import Data.Functor (class Functor, map)
import Data.Maybe (Maybe(..), isJust, fromJust)
import Data.Tuple (Tuple(..), snd, uncurry)
import Partial.Unsafe (unsafePartialBecause)
import Prelude ((-), (+), ($), (<<<), (<=), otherwise, flip)

data InfiniteListF a b = ILF (b -> a) (b -> b) b

newtype InfiniteList a = IL (Exists (InfiniteListF a))

fRunExists :: forall f r. Exists f -> (forall a. f a -> r) -> r
fRunExists = flip runExists

-- | The datatype that represents an infinite list. The two type arguments are:
-- |
-- | * `a`: The type that will be returned by the getter methods.
-- | * `b`: The base type for this infinite list that is being iterated over.
--data InfiniteList a b = IL (b -> a) (b -> b) b


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

instance infiniteFunctor :: Functor InfiniteList where
  map f (IL il) = IL (mkExists (fRunExists il (map' f)))

map' :: forall a b c. (a -> b) -> InfiniteListF a c -> InfiniteListF b c
map' f' (ILF conv iter start) = ILF (f' <<< conv) iter start

instance infiniteFilterable :: F.Filterable (InfiniteListF a) where
  partitionMap = partitionMapInternal

  partition = partitionInternal

  filterMap = filterMapInternal

  filter = filterInternal

partitionMapInternal :: forall a b l r. (a -> E.Either l r) -> InfiniteListF a b -> { left :: InfiniteListF b l, right :: InfiniteListF b r }
partitionMapInternal f il = {
    left: fromLeft applied,
    right: fromRight applied
  }
  where
    applied = map f il

fromLeft :: forall a b c. InfiniteListF (E.Either a c) b -> InfiniteListF a b
fromLeft = map (unsafePartialBecause "We have filtered on only the left elements" E.fromLeft) <<< filterInternal E.isLeft

fromRight :: forall a b c. InfiniteListF (E.Either a c) b -> InfiniteListF a b
fromRight = map (unsafePartialBecause "We have filtered on only the right elements" E.fromRight) <<< filterInternal E.isRight

partitionInternal :: forall a b. (a -> Boolean) -> InfiniteListF a b -> { no :: InfiniteListF a b, yes :: InfiniteListF a b }
partitionInternal p xs = let o = partitionMapInternal (F.eitherBool p) xs
                         in {no: o.left, yes: o.right}

filterMapInternal :: forall a b c. (a -> Maybe c) -> InfiniteListF a b -> InfiniteListF c b
filterMapInternal f = catMaybes <<< map f

catMaybes :: forall b a. InfiniteListF (Maybe a) b -> InfiniteListF a b
catMaybes = map (unsafePartialBecause "We have filtered on only the Just elements" fromJust) <<< filterInternal isJust

filterInternal :: forall a b. (a -> Boolean) -> InfiniteListF a b -> InfiniteListF a b
filterInternal matches (ILF conv iter start) = ILF conv next nextStart
  where
    nextStart = if matches' start then start else next start

    next x = let step = iter x in
             if matches' step then step else next step

    matches' = matches <<< conv

-- We could possibly implement Apply if the need for it was there

-- ### Generators

-- | Generate an infinite list by providing:
-- |
-- |  * a function to iterate from the previous value in the infinite list to the next AND
-- |  * the first element in the infinite list to begin the iteration.
-- |
-- | For example, to generate the list of natural numbers you can do the following:
-- |
-- |     naturals = iterate (_ + 1) 1
-- |
-- | In english, "The natural numbers start at 1 and you can get the next natural
-- | number by adding one to the prior number".
-- |
-- | Running time: `O(1)`
iterate :: forall a. (a -> a) -> a -> InfiniteListF a a
iterate iter start = ILF id iter start

-- | Generate an infinite list by infinitely repeating a provided non-empty list.
-- |
-- | The list that is provided must be non-empty because it is impossible to repeat
-- | an empty list and end up with a list with something in it.
-- |
-- | To see how this method works see the following example:
-- |
-- |     > take 15 $ repeat (0 :| [2, 1, 3])
-- |     [0,2,1,3,0,2,1,3,0,2,1,3,0,2,1]
-- |
-- |     >
-- |
-- | This is an infinite repeat of the list 0, 2, 1, 3.
-- |
-- | Note: You will notice that an `InfiniteList (Tuple Int a) a` is returned, the
-- | reason for the tuple in the type is that the non-empty list that is passed in
-- | is indexed so that we can always tell what the next element should be from a
-- | prior element.
-- |
-- | Running time: `O(1)`
repeat :: forall a. NE.NonEmpty Array a -> InfiniteListF a (Tuple Int a)
repeat oxs = ILF snd iter firstVal
  where
    ref = index 0 (NE.oneOf $ oxs)

    index :: Int -> Array a -> Array (Tuple Int a)
    index n values = case A.uncons values of
      Just {head: x, tail: xs} -> Tuple n x `A.cons` index (n + 1) xs
      Nothing -> []

    iter :: Tuple Int a -> Tuple Int a
    iter (Tuple cIndex _) = case A.index ref (cIndex + 1) of
      Just x -> x
      Nothing -> firstVal

    firstVal = Tuple 0 (NE.head oxs)

-- | Get the first element of an infinite list. For example:
-- |
-- |     (head $ iterate (_ + 1) 1) `shouldEqual` 1
-- |
-- | The first value in this example infinite list will be a one.
-- |
-- | The maximum performance cost of this method will be however much it takes to
-- | convert a value of type `b` to type `a`.
head :: forall a b. InfiniteListF a b -> a
head (ILF conv _ x) = conv x

-- | Return the infinite list without the first element. For example:
-- |
-- |     (head <<< tail $ iterate (_ + 1) 1) `shouldEqual` 2
-- |
-- | In this example, we generate the list of natural numbers, drop the first element
-- | and get then following element. This will return the second element of the natural numbers
-- | and, naturally, it is two.
-- |
-- | The maximum performance cost of this method will be however much it takes to
-- | iterate from the previous value of `b` to the next value of `b`.
tail :: forall a b. InfiniteListF a b -> InfiniteListF a b
tail il = (uncons il).tail

-- | Returns an object that contains two values: `head` and `tail`. These two
-- | values will contain the same data as if you had called the respective head
-- | and tail functions on the original list. This method should only be used as
-- | a convenience method to simplify the head and tail calls.
-- |
-- | Semantically speaking, this method separates the first element of an infinite
-- | list from the remaining elements of the infinite list.
-- |
-- | For example:
-- |
-- |     let s = uncons $ iterate (_ + 1) 1
-- |     s.head `shouldEqual` 1
-- |     head s.tail `shouldEqual` 2
-- |
-- | The performance cost of this method should be the summation of the respective
-- | head and tail calls that you would need to make to get the same result.
uncons :: forall a b. InfiniteListF a b -> { head :: a, tail :: InfiniteListF a b }
uncons (ILF conv iter start) = { head: conv start, tail: ILF conv iter (iter start) }

-- | Take the first 'n' elements from an infinite list.
-- |
-- | For example:
-- |
-- |     take 10 (iterate (_ + 1) 1) `shouldEqual` [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
-- |
-- | The performance cost of this function will be approximately O(n) * m where m
-- | is the cost of performing n uncons operations.
take :: forall a b. Int -> InfiniteListF a b -> Array a
take = _take uncons
-- TODO when Tail Call Optimization modulo cons is implemented then use this instead: https://github.com/purescript/purescript/issues/2154
{-
take on oil = L.toUnfoldable $ go on oil
  where
    go :: forall a b. Int -> InfiniteList a b -> L.List a
    go n il | n <= 0    = L.Nil
            | otherwise = head il `L.Cons` go (n - 1) (tail il)
-}

foreign import _take :: forall a b. (InfiniteListF a b -> { head :: a, tail :: InfiniteListF a b }) -> Int -> InfiniteListF a b -> Array a

-- | Drop the first 'n' elements from an infinite list and return the resultant infinite list.
-- |
-- | For example:
-- |
-- |     head <<< drop 10 $ (iterate (_ + 1) 1) `shouldEqual` 11
-- |
-- | The performance cost of this function will be approximately O(n) * m where m is the
-- | cost of performing one tail operation.
drop :: forall a b. Int -> InfiniteListF a b -> InfiniteListF b a
drop n il | n <= 0    = il
          | otherwise = drop (n - 1) (tail il)

-- | Return all of the elements at the start of the infinite list that match the
-- | provided condition.
-- |
-- | For example, to get all of the numbers that, when squared, are less than a thousand:
-- |
-- |     takeWhile (\x -> (x `pow` 2) < 128) $ iterate (_ + 1) 1 `shouldEqual` [1,2,3,4,5,6,7,8,9,10,11]
takeWhile :: forall a b. (a -> Boolean) -> InfiniteListF a b -> Array a
takeWhile = _takeWhile uncons
-- TODO when Tail Call Optimization modulo cons is implemented then use this instead: https://github.com/purescript/purescript/issues/2154
{-
takeWhile :: forall a b. (a -> Boolean) -> InfiniteList a b -> Array a
takeWhile keep = L.toUnfoldable <<< go
  where
    go :: InfiniteList a b -> L.List a
    go il = let u = uncons il
            in if keep u.head then u.head `L.Cons` go u.tail else L.Nil
-}

foreign import _takeWhile :: forall a b. (InfiniteListF a b -> { head :: a, tail :: InfiniteListF a b }) -> (a -> Boolean) -> InfiniteListF a b -> Array a

-- | Drop all of the elements from the start of an infinite list that match the
-- | provided condition.
-- |
-- | For example, to drop all of the elements from an infinite list who have log
-- | values of less than 10:
-- |
-- |     (head <<< dropWhile (\x -> log x < 10.0) $ iterate (_ + 1.0) 1.0) `shouldEqual` 22027.0
dropWhile :: forall a b. (a -> Boolean) -> InfiniteListF a b -> InfiniteListF b a
dropWhile discard = go
  where
    go :: InfiniteListF a b -> InfiniteListF b a
    go il = if discard (head il) then go (tail il) else il

-- | Zip two infinite lists together.
-- |
-- | The resultant infinite list returns tuples of the original list values. For example:
-- |
-- |     let increasing = iterate (_ + 1)
-- |     let pairs = zip (increasing 0) (increasing 1)
-- |     take 5 pairs `shouldEqual` [(Tuple 0 1),(Tuple 1 2),(Tuple 2 3),(Tuple 3 4),(Tuple 4 5)]
-- |
-- | Running time: O(1)
zip :: forall a b c d. InfiniteList a -> InfiniteList b -> InfiniteList (Tuple a b)
zip = ilfRunExists zip'

ilfRunExists :: forall a b c d e f. (InfiniteListF a c -> InfiniteListF b d -> InfiniteListF e f) -> InfiniteList a -> InfiniteList b -> InfiniteList e
ilfRunExists (IL ila) (IL ilb) runILF = mkExists runA
  where
    runA = fRunExists ila runB
    runB ilfA = fRunExists ilb (\ilfB -> runILF ilfA ilfB)

zip' :: forall a b c d. InfiniteListF c a -> InfiniteListF d b -> InfiniteListF (Tuple c d) (Tuple a b)
zip' (ILF lConv lIter lStart) (ILF rConv rIter rStart) = ILF conv iter start
  where
    conv (Tuple x y) = Tuple (lConv x) (rConv y)
    iter (Tuple x y) = Tuple (lIter x) (rIter y)
    start = Tuple lStart rStart

-- | Zip two infinite lists together with a data joining function.
-- |
-- | For example:
-- |
-- |     let increasing = iterate (_ + 1)
-- |     let items = zipWith (\n x -> n <> show x) (iterate id "Name: ") (increasing 1)
-- |     take 5 items `shouldEqual` ["Name: 1","Name: 2","Name: 3","Name: 4","Name: 5"]
zipWith :: forall a b c. (a -> b -> c) -> InfiniteList a -> InfiniteList b -> InfiniteList c
zipWith f a = map (uncurry f) <<< zip a
