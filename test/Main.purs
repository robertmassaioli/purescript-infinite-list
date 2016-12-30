module Test.Main where

import Test.Spec                  (describe, it, pending)
import Test.Spec.Assertions       (shouldEqual)
import Test.Spec.Runner           (run)
import Test.Spec.Reporter.Console (consoleReporter)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Node.Process (PROCESS)

import Prelude
import Data.InfiniteList
import Data.List as L
import Data.NonEmpty ((:|))
import Data.Tuple
import Data.Filterable (filter)

main :: forall e. Eff (console :: CONSOLE, process :: PROCESS | e) Unit
main = run [consoleReporter] allSpecs

-- main :: forall e. Eff (console :: CONSOLE | e) Unit
allSpecs = describe "Infinite List" do
  iterateSpecs
  repeatedSpecs
  dropWhileSpecs
  takeWhileSpecs
  functorSpecs
  filterableSpecs

iterateSpecs = describe "Iteated infinite lists" do
  it "The natural numbers" do
    let il = iterate ((+) 1) 1
    head il `shouldEqual` 1
    (head <<< tail $ il) `shouldEqual` 2
    (head <<< tail <<< tail $ il) `shouldEqual` 3
    (head <<< tail <<< tail <<< tail $ il) `shouldEqual` 4
    (head <<< tail <<< tail <<< tail <<< tail $ il) `shouldEqual` 5
  it "Created a descending list of negative numbers" do
    let il = iterate (\x -> x - 1) 0
    head il `shouldEqual` 0
    (head <<< tail $ il) `shouldEqual` (-1)
    (head <<< tail <<< tail $ il) `shouldEqual` (-2)
    (head <<< tail <<< tail <<< tail $ il) `shouldEqual` (-3)
    (head <<< tail <<< tail <<< tail <<< tail $ il) `shouldEqual` (-4)
  it "From a thousand and beyond" do
    let il = iterate ((+) 1) 1000
    head il `shouldEqual` 1000
    (head <<< tail $ il) `shouldEqual` 1001
    (head <<< tail <<< tail $ il) `shouldEqual` 1002
    (head <<< tail <<< tail <<< tail $ il) `shouldEqual` 1003
    (head <<< tail <<< tail <<< tail <<< tail $ il) `shouldEqual` 1004
  it "create the binary list" do
    let il = iterate not true
    head il `shouldEqual` true
    (head <<< tail $ il) `shouldEqual` false
    (head <<< tail <<< tail $ il) `shouldEqual` true
    (head <<< tail <<< tail <<< tail $ il) `shouldEqual` false

repeatedSpecs = describe "Repeated infinite lists" do
  it "The numbers from one to three repeated" do
    let il = repeat (1 :| L.fromFoldable [2, 3])
    take 8 il `shouldEqual` L.fromFoldable [1, 2, 3, 1, 2, 3, 1, 2]

  it "strange numbers repeated work" do
    let il = repeat (1 :| L.fromFoldable [5, 2, 1, 3, 4])
    take 8 il `shouldEqual` L.fromFoldable [1, 5, 2, 1, 3, 4, 1, 5]

dropWhileSpecs = describe "dropWhile" do
  it "'head' after a 'dropWhile' should return something that matches the condition" do
    let il = iterate ((+) 1) 0
    let result = dropWhile (\x -> x < 10) il
    head il `shouldEqual` 0
    head result `shouldEqual` 10
  it "dropWhile should skip many elements" do
    let il = iterate ((+) 1) 0
    let result = dropWhile (\x -> x < 100000) il
    head result `shouldEqual` 100000

takeWhileSpecs = describe "takeWhile" do
  it "takeWhile should generate a good list of elements" do
    let il = iterate ((+) 1) 0
    takeWhile (\x -> x < 10) il `shouldEqual` L.fromFoldable [0, 1, 2, 3, 4, 5, 6, 7, 8 ,9]
  it "takeWhile on many elements should be fast" do
    let il = iterate ((+) 1) 0
    let result = takeWhile (\x -> x < 10000) il
    L.length result `shouldEqual` 10000

functorSpecs = describe "Functor" do
  it "can increment an infinite list by five" do
    let il = iterate ((+) 1) 0
    let result = map ((+) 5) il
    take 5 result `shouldEqual` L.fromFoldable [5, 6, 7, 8, 9]
  it "can map an infinite list to boolean values" do
    let il = iterate ((+) 1) 0
    let result = map (\x -> x `mod` 3 == 0) il
    take 5 result `shouldEqual` L.fromFoldable [true, false, false, true, false]
  it "can map an infinite list into tuple values" do
    let il = iterate ((+) 1) 0
    let result = map (\x -> Tuple x ("n: " <> show x)) il
    take 5 result `shouldEqual` L.fromFoldable [Tuple 0 "n: 0", Tuple 1 "n: 1", Tuple 2 "n: 2", Tuple 3 "n: 3", Tuple 4 "n: 4"]

filterableSpecs = describe "Filterable" do
  it "can filter numbers from an infinite list" do
    let il = iterate ((+) 1) 1
    let result = filter (\x -> (x `mod` 3 == 0) || (x `mod` 5 == 0)) il
    takeWhile (\x -> x <= 20) result `shouldEqual` L.fromFoldable [3, 5, 6, 9, 10, 12, 15, 18, 20]
