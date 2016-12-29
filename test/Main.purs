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
    take 5 il `shouldEqual` L.fromFoldable [1, 5, 2, 1, 3]

dropWhileSpecs = describe "dropWhile" do
  pending "The 'head' after a 'dropWhile' should return something that matches the condition"
  pending "dropWhile should skip many elements"

takeWhileSpecs = describe "takeWhile" do
  pending "takeWhile should generate a good list of elements"

functorSpecs = describe "Functor" do
  pending "can increment an infinite list by five"
  pending "can map an infinite list to boolean values"
  pending "can map an infinite list into tuple values"

filterableSpecs = describe "Filterable" do
  pending "can filter numbers from an infinite list"

-- Iterate
-- Repeat
{-
, iterate
, repeat
, take
, drop
, takeWhile
, dropWhile
, head
, tail
, zip
, map
, filter
-}
