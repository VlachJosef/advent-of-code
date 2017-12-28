module Test.Year2017.Day14 where

-- http://adventofcode.com/2017/day/14

import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Data.Tuple.Nested ((/\))
import Prelude (Unit, ($), (<>), (==))
import Year2017.Day14 (solution1, solution2)
import Year2017.Day14.Input (testInput, input)

day14Tests :: forall e. Eff (console :: CONSOLE | e) Unit
day14Tests = foreachE (tests) (\(f /\ steps /\ expected) ->
                              let
                                res = f steps
                              in do
                                log $ show (res == expected)
                                  <> " --- group size"
                                  <> " should be: "
                                  <> show expected
                                  <> ", was "
                                  <> show res) where

  tests = map (solution1 /\ _) [ testInput /\ 8108
                               , input     /\ 8230
                               ]

day14Tests2 :: forall e. Eff (console :: CONSOLE | e) Unit
day14Tests2 = foreachE (tests) (\(f /\ steps /\ expected) ->
                              let
                                res = f steps
                              in do
                                log $ show (res == expected)
                                  <> " --- group size"
                                  <> " should be: "
                                  <> show expected
                                  <> ", was "
                                  <> show res
                               ) where

  tests = map (solution2 /\ _) [ testInput /\ Just 1242
                               , input     /\ Just 1103
                               ]
