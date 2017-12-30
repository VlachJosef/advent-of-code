module Test.Year2017.Day16 where

-- http://adventofcode.com/2017/day/16

import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Functor (map)
import Data.Show (show)
import Data.Tuple.Nested ((/\))
import Prelude (Unit, ($), (<>), (==))
import Year2017.Day16 (solution1, solution2, promenadeAE, promenadeAP)
import Year2017.Day16.Input (testInput, input)

day16Tests :: forall e. Eff (console :: CONSOLE | e) Unit
day16Tests = foreachE (tests) (\(f /\ inputs /\ promenade /\ expected) ->
                              let
                                res = f promenade inputs
                              in do
                                log $ show (res == expected)
                                  <> " ---"
                                  <> " should be: "
                                  <> show expected
                                  <> ", was "
                                  <> show res) where

  tests = map (solution1 /\ _) [ testInput /\ promenadeAE /\ "baedc"
                               , input     /\ promenadeAP /\ "jkmflcgpdbonihea"
                               ]

day16Tests2 :: forall e. Eff (console :: CONSOLE | e) Unit
day16Tests2 = foreachE (tests) (\(f /\ inputs /\ promenade /\ expected) ->
                              let
                                res = f promenade inputs
                              in do
                                log $ show (res == expected)
                                  <> " ---"
                                  <> " should be: "
                                  <> show expected
                                  <> ", was "
                                  <> show res
                               ) where

  tests = map (solution2 /\ _) [ testInput /\ promenadeAE /\ "abcde"
                               , input     /\ promenadeAP /\ "ajcdefghpkblmion"
                               ]
