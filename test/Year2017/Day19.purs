module Test.Year2017.Day19 where

-- http://adventofcode.com/2017/day/19

import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Data.Tuple.Nested ((/\))
import Prelude (Unit, ($), (<>), (==))
import Year2017.Day19 (solution1, solution2)
import Year2017.Day19.Input (testInput, input)

day19Tests :: forall e. Eff (console :: CONSOLE | e) Unit
day19Tests = foreachE (tests) (\(f /\ inputs /\ expected) ->
                              let
                                res = f inputs
                              in do
                                log $ show (res == expected)
                                  <> " ---"
                                  <> " should be: "
                                  <> show expected
                                  <> ", was "
                                  <> show res) where

  tests = map (solution1 /\ _) [ testInput /\ Just "ABCDEF"
                               , input     /\ Just "GPALMJSOY"
                               ]

day19Tests2 :: forall e. Eff (console :: CONSOLE | e) Unit
day19Tests2 = foreachE (tests) (\(f /\ inputs /\ expected) ->
                              let
                                res = f inputs
                              in do
                                log $ show (res == expected)
                                  <> " ---"
                                  <> " should be: "
                                  <> show expected
                                  <> ", was "
                                  <> show res) where

  tests = map (solution2 /\ _) [ testInput /\ Just 38
                               , input     /\ Just 16204
                               ]
