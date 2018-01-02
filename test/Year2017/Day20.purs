module Test.Year2017.Day20 where

-- http://adventofcode.com/2017/day/20

import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (Either(..))
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Data.Tuple.Nested ((/\))
import Prelude (Unit, ($), (<>), (==))
import Year2017.Day20 (solution1, solution2)
import Year2017.Day20.Input (testInput, testInputCollision, input)

day20Tests :: forall e. Eff (console :: CONSOLE | e) Unit
day20Tests = foreachE (tests) (\(f /\ inputs /\ expected) ->
                              let
                                res = f inputs 330
                              in do
                                log $ show (res == expected)
                                  <> " ---"
                                  <> " should be: "
                                  <> show expected
                                  <> ", was "
                                  <> show res) where

  tests = map (solution1 /\ _) [ testInput /\ (Right $ Just 0)
                               , input     /\ (Right $ Just 150)
                               ]

day20Tests2 :: forall e. Eff (console :: CONSOLE | e) Unit
day20Tests2 = foreachE (tests) (\(f /\ inputs /\ expected) ->
                              let
                                res = f inputs 39
                              in do
                                log $ show (res == expected)
                                  <> " ---"
                                  <> " should be: "
                                  <> show expected
                                  <> ", was "
                                  <> show res) where

  tests = map (solution2 /\ _) [ testInputCollision /\ (Right $ 1)
                               , testInput          /\ (Right $ 2)
                               , input              /\ (Right $ 657)
                               ]
