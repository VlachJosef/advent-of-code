module Test.Year2017.Day5 where

import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array (take)
import Data.Functor (map)
import Data.Show (show)
import Data.Tuple.Nested (type (/\), (/\))
import Prelude (Unit, (==), (<>), ($))
import Year2017.Day5 (Instructions, input, inputTest, solution1, solution2)

day5Tests :: forall e. Eff (console :: CONSOLE | e) Unit
day5Tests = foreachE (solution1Tests <> solution2Tests) (\(f /\ testee /\ expected) -> do
                                                            log $ show (f testee == expected) <> " --- " <> show (take 10 testee) <> " should be " <> show expected <> ", was " <> show (f testee)) where
  solution1Tests :: Array ((Instructions -> Int) /\ Instructions /\ Int)
  solution1Tests = map (solution1 /\ _) [
    inputTest /\ 5,
    input     /\ 336905]

  solution2Tests :: Array ((Instructions -> Int) /\ Instructions /\ Int)
  solution2Tests = map (solution2 /\ _) [
    inputTest /\ 10
    --input   /\ 21985262 -- takes lots of time to run
  ]
