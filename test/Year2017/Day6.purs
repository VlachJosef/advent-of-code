module Test.Year2017.Day6 where

import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array (take)
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Data.Tuple.Nested (type (/\), (/\))
import Prelude (Unit, (==), (<>), ($))
import Year2017.Day6 (MemoryState, input, inputTest, redistributeBlocks, solution)

day6Tests :: forall e. Eff (console :: CONSOLE | e) Unit
day6Tests = foreachE (solution1Tests) (\(f /\ idx /\ residue /\ expected) -> do
                                          log $ show (f idx residue 4  == expected) <> " --- " <> show (idx) <> " should be " <> show expected <> ", was " <> show (f idx residue 4)) where
  solution1Tests :: Array ((Int -> Int -> Int -> Array Int) /\ Int /\ Int /\ Array Int)
  solution1Tests = map (redistributeBlocks /\ _) [
    1 /\ 0 /\ [0, 0, 0, 0],
    1 /\ 1 /\ [0, 1, 0, 0],
    1 /\ 2 /\ [0, 1, 1, 0],
    1 /\ 3 /\ [0, 1, 1, 1],
    2 /\ 0 /\ [0, 0, 0, 0],
    2 /\ 1 /\ [0, 0, 1, 0],
    2 /\ 2 /\ [0, 0, 1, 1],
    2 /\ 3 /\ [1, 0, 1, 1],
    3 /\ 0 /\ [0, 0, 0, 0],
    3 /\ 1 /\ [0, 0, 0, 1],
    3 /\ 2 /\ [1, 0, 0, 1],
    3 /\ 3 /\ [1, 1, 0, 1],
    4 /\ 0 /\ [0, 0, 0, 0],
    4 /\ 1 /\ [1, 0, 0, 0],
    4 /\ 2 /\ [1, 1, 0, 0],
    4 /\ 3 /\ [1, 1, 1, 0]
  ]

day6Tests2 :: forall e. Eff (console :: CONSOLE | e) Unit
day6Tests2 = foreachE (solution2Tests) (\(f /\ memory /\ expectedLoopSize /\ expectedCycleCount) -> do
                                           log $ show (f memory == Just (expectedLoopSize /\ expectedCycleCount)) <> " --- " <> show memory <> " should be " <> show (Just (expectedLoopSize /\ expectedCycleCount)) <> ", was " <> show (f memory)) where
  solution2Tests :: Array ((MemoryState -> Maybe (Int /\ Int)) /\ MemoryState /\ Int /\ Int)
  solution2Tests = map (solution /\ _) [
    inputTest /\ 4 /\ 5,
    input     /\ 2392 /\ 6681
  ]
