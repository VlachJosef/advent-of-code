module Test.Year2017.Day15 where

-- http://adventofcode.com/2017/day/15

import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Functor (map)
import Data.Show (show)
import Data.Tuple.Nested ((/\))
import Prelude (Unit, ($), (<>), (==))
import Year2017.Day15 (solution1, solution2)
import Year2017.Day15.Input (testInputA, testInputB, inputA, inputB)

day15Tests :: forall e. Eff (console :: CONSOLE | e) Unit
day15Tests = foreachE (tests) (\(f /\ ia /\ ib /\ expected) ->
                              let
                                res = f ia ib 40000000
                              in do
                                log $ show (res == expected)
                                  <> " ---"
                                  <> " should be: "
                                  <> show expected
                                  <> ", was "
                                  <> show res) where

  tests = map (solution1 /\ _) [ testInputA /\ testInputB /\ 588
                               , inputA     /\ inputB     /\ 626
                               ]

day15Tests2 :: forall e. Eff (console :: CONSOLE | e) Unit
day15Tests2 = foreachE (tests) (\(f /\ ia /\ ib /\ expected) ->
                              let
                                res = f ia ib 5000000
                              in do
                                log $ show (res == expected)
                                  <> " ---"
                                  <> " should be: "
                                  <> show expected
                                  <> ", was "
                                  <> show res
                               ) where

  tests = map (solution2 /\ _) [ testInputA /\ testInputB /\ 309
                               , inputA     /\ inputB     /\ 306
                               ]
