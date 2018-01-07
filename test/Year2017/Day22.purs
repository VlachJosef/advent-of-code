module Test.Year2017.Day22 where

-- http://adventofcode.com/2017/day/22

import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (Either(..))
import Data.Functor (map)
import Data.Show (show)
import Data.Tuple.Nested ((/\))
import Prelude (Unit, ($), (<>), (==))
import Year2017.Day22 (solution1, solution2)
import Year2017.Day22.Input (testInput, input)

day22Tests :: forall e. Eff (console :: CONSOLE | e) Unit
day22Tests = foreachE (tests) (\(f /\ inputs /\ burst /\ expected) ->
                              let
                                res = f inputs burst
                              in do
                                log $ show (res == expected)
                                  <> " ---"
                                  <> " should be: "
                                  <> show expected
                                  <> ", was "
                                  <> show res) where

  tests = map (solution1 /\ _) [ testInput /\ 10000 /\ (Right 5587)
                               , input     /\ 10000 /\ (Right 5196)
                               ]

day22Tests2 :: forall e. Eff (console :: CONSOLE | e) Unit
day22Tests2 = foreachE (tests) (\(f /\ inputs /\ burst /\ expected) ->
                              let
                                res = f inputs burst
                              in do
                                log $ show (res == expected)
                                  <> " ---"
                                  <> " should be: "
                                  <> show expected
                                  <> ", was "
                                  <> show res) where

  tests = map (solution2 /\ _) [ testInput /\ 10000000 /\ (Right 2511944)
                               , input     /\ 10000000 /\ (Right 2511633)
                               ]
