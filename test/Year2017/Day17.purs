module Test.Year2017.Day17 where

-- http://adventofcode.com/2017/day/17

import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Data.Tuple.Nested ((/\))
import Prelude (Unit, ($), (<>), (==))
import Year2017.Day17 (solution1, solution2)
import Year2017.Day17.Input (testInput, input)

day17Tests :: forall e. Eff (console :: CONSOLE | e) Unit
day17Tests = foreachE (tests) (\(f /\ inputs /\ expected) ->
                              let
                                res = f inputs
                              in do
                                log $ show (res == expected)
                                  <> " ---"
                                  <> " should be: "
                                  <> show expected
                                  <> ", was "
                                  <> show res) where

  tests = map (solution1 /\ _) [ testInput /\ (Just 638)
                               , input     /\ (Just 2000)
                               ]

day17Tests2 :: forall e. Eff (console :: CONSOLE | e) Unit
day17Tests2 = foreachE (tests) (\(f /\ inputs /\ expected) ->
                              let
                                res = f 50000000 inputs
                              in do
                                log $ show (res == expected)
                                  <> " ---"
                                  <> " should be: "
                                  <> show expected
                                  <> ", was "
                                  <> show res
                               ) where

  tests = map (solution2 /\ _) [ testInput /\ 1222153
                               , input     /\ 10242889
                               ]
