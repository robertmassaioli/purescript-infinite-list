module Test.Main where

import Test.Spec                  (describe, it, pending)
import Test.Spec.Assertions       (shouldEqual)
import Test.Spec.Runner           (run)
import Test.Spec.Reporter.Console (consoleReporter)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Node.Process (PROCESS)

import Prelude

main :: forall e. Eff (console :: CONSOLE, process :: PROCESS | e) Unit
main = run [consoleReporter] allSpecs

-- main :: forall e. Eff (console :: CONSOLE | e) Unit
allSpecs = describe "Infinite List" do
  pending "Created a list of the natural numbers valid for the first five"
  pending "Created a descending list of negative numbers"
  pending "The 'head' after a 'dropWhile' should return something that matches the condition"
  pending "dropWhile should skip many elements"
  pending "takeWhile should generate a good list of elements"
