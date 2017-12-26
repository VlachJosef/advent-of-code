module Test.Year2017.Day12 where

import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Functor (map)
import Data.Show (show)
import Data.Tuple.Nested ((/\))
import Prelude (Unit, ($), (<>), (==))
import Year2017.Day12 (solution1, solution2)
import Year2017.Day12.Input (input, testInput)

day12Tests :: forall e. Eff (console :: CONSOLE | e) Unit
day12Tests = foreachE (tests) (\(f /\ steps /\ expected) ->
                              let
                                res = f steps
                              in do
                                log $ show (res == expected)
                                  <> " --- group size"
                                  <> " res: "
                                  <> show res
                                  <> " should be: "
                                  <> show expected
                                  <> ", was "
                                  <> show res) where

  tests = map (solution1 /\ _) [ testInput /\ 6
                               , input     /\ 128
                               ]

day12Tests2 :: forall e. Eff (console :: CONSOLE | e) Unit
day12Tests2 = foreachE (tests) (\(f /\ steps /\ expected) ->
                              let
                                res = f steps
                              in do
                                log $ show (res == expected)
                                  <> " --- groups count"
                                  <> " res: "
                                  <> show res
                                  <> " should be: "
                                  <> show expected
                                  <> ", was "
                                  <> show res) where

  tests = map (solution2 /\ _) [ testInput /\ 2
                               , input     /\ 209
                               ]
