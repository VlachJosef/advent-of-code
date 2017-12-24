module Test.Year2017.Day10 where

import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Functor (map)
import Data.Newtype (unwrap, wrap)
import Data.Show (show)
import Data.Tuple.Nested ((/\))
import Prelude (Unit, ($), (<>), (==))
import Year2017.Day10 (lengthsAsString, solution, solution2, step)
import Year2017.Day10.Input (input, lengths, testInput, testLengths)
import Year2017.Day10.Types (DenseHash(..))

day10Tests :: forall e. Eff (console :: CONSOLE | e) Unit
day10Tests = foreachE (tests) (\(f /\ inputs /\ currentPosition /\ len /\ expected) ->
                              let
                                res = f (wrap inputs) (wrap currentPosition) (wrap len)
                              in do
                                log $ show (unwrap res == expected)
                                  <> " --- "
                                  <> show inputs
                                  <> ", cp: "
                                  <> show currentPosition
                                  <> ", lenght: "
                                  <> show len
                                  <> " should be: "
                                  <> show expected
                                  <> ", was "
                                  <> show res) where

  tests = map (step /\ _) [ [0, 1, 2, 3, 4] /\ 0 /\ 3 /\ [2, 1, 0, 3, 4]
                          , [0, 1, 2, 3, 4] /\ 1 /\ 2 /\ [0, 2, 1, 3, 4]
                          , [0, 1, 2, 3, 4] /\ 1 /\ 3 /\ [0, 3, 2, 1, 4]
                          , [0, 1, 2, 3, 4] /\ 3 /\ 3 /\ [3, 1, 2, 0, 4]
                          , [0, 1, 2, 3, 4] /\ 4 /\ 4 /\ [1, 0, 4, 3, 2]
                          ]


day10Tests2 :: forall e. Eff (console :: CONSOLE | e) Unit
day10Tests2 = foreachE (tests) (\(f /\ inputs /\ lengths /\ expected) ->
                              let
                                res = f inputs lengths
                              in do
                                log $ show (res == expected)
                                  <> " --- "
                                  <> show inputs
                                  <> ", lengths: "
                                  <> show lengths
                                  <> " should be: "
                                  <> show expected
                                  <> ", was "
                                  <> show res) where

  tests = map (solution /\ _) [ testInput /\ testLengths /\ 12
                              , input     /\ lengths     /\ 20056
                              ]

day10Tests3 :: forall e. Eff (console :: CONSOLE | e) Unit
day10Tests3 = foreachE (tests) (\(f /\ inputs /\ expected) ->
                              let
                                DenseHash res = f inputs
                              in do
                                log $ show (res == expected)
                                  <> " --- hash for "
                                  <> show inputs
                                  <> " should be: "
                                  <> show expected
                                  <> ", was "
                                  <> show res) where

  tests = map (solution2 /\ _) [ ""                     /\ "a2582a3a0e66e6e86e3812dcb672a272"
                              , "AoC 2017"              /\ "33efeb34ea91902bb2f59c9920caa6cd"
                              , "1,2,3"                 /\ "3efbe78a8d82f29979031a4aa0b16a9d"
                              , "1,2,4"
                                /\ "63960835bcdc130f0b66d7ff4f6a5a8e"
                              , lengthsAsString lengths /\ "d9a7de4a809c56bf3a9465cb84392c8e"
                              ]
