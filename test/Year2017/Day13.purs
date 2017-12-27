module Test.Year2017.Day13 where

import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Functor (map)
import Data.Show (show)
import Data.Tuple.Nested ((/\))
import Prelude (Unit, ($), (<>), (==))
import Year2017.Day13 (solution1, solution2)
import Year2017.Day13.Input (input, testInput)

day13Tests :: forall e. Eff (console :: CONSOLE | e) Unit
day13Tests = foreachE (tests) (\(f /\ steps /\ expected) ->
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

  tests = map (solution1 /\ _) [ testInput /\ 24
                               , input     /\ 1504
                               ]

day13Tests2 :: forall e. Eff (console :: CONSOLE | e) Unit
day13Tests2 = foreachE (tests) (\(f /\ steps /\ expected) ->
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

  tests = map (solution2 /\ _) [ testInput /\ 10
                               --, input     /\ 3823370 -- Tea break
                               ]
