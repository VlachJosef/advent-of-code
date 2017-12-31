module Test.Year2017.Day18 where

-- http://adventofcode.com/2017/day/18

import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (Either(..))
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Data.Tuple.Nested ((/\))
import Prelude (Unit, ($), (<>), (==))
import Year2017.Day18 (solution1, solution2)
import Year2017.Day18.Input (testInput, input)

day18Tests :: forall e. Eff (console :: CONSOLE | e) Unit
day18Tests = foreachE (tests) (\(f /\ inputs /\ expected) ->
                              let
                                res = f inputs
                              in do
                                log $ show (res == expected)
                                  <> " ---"
                                  <> " should be: "
                                  <> show expected
                                  <> ", was "
                                  <> show res) where

  tests = map (solution1 /\ _) [ testInput /\ (Right (Just 4.0))
                               , input     /\ (Right (Just 2951.0))
                               ]

day18Tests2 :: forall e. Eff (console :: CONSOLE | e) Unit
day18Tests2 = foreachE (tests) (\(f /\ inputs /\ expected) ->
                              let
                                res = f inputs
                              in do
                                log $ show (res == expected)
                                  <> " ---"
                                  <> " should be: "
                                  <> show expected
                                  <> ", was "
                                  <> show res) where

  tests = map (solution2 /\ _) [ testInput /\ (Right (Just 1))
                               , input     /\ (Right (Just 7366))
                               ]
