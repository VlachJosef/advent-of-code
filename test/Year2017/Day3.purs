module Test.Year2017.Day3 where

import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Data.Tuple.Nested (type (/\), (/\))
import Prelude (Unit, (==), (<>), ($))
import Year2017.Day3 (solution1, solution2, neighbours)

day3Tests :: forall e. Eff (console :: CONSOLE | e) Unit
day3Tests = foreachE (solution1Tests <> solution2Tests) (\(f /\ testee /\ expected) -> do
                                                            log $ show (f testee == expected) <> " --- " <> show testee <> " should be " <> show expected <> ", was " <> show (f testee)) where
  solution1Tests :: Array ((Int -> Int) /\ Int /\ Int)
  solution1Tests = map (solution1 /\ _)[
    1      /\ 0,
    12     /\ 3,
    23     /\ 2,
    1024   /\ 31,
    312051 /\ 430
  ]

  solution2Tests :: Array ((Int -> Int) /\ Int /\ Int)
  solution2Tests = map (solution2 /\ _)[
    312051 /\ 312453
  ]

day3NeighboursTests :: forall e. Eff (console :: CONSOLE | e) Unit
day3NeighboursTests = foreachE (neighborhoodTests) (\(f /\ testee /\ expected) -> do
                                                            log $ show (f testee == Just expected) <> " --- " <> show testee <> " should be " <> show expected <> ", was " <> show (f testee)) where

  neighborhoodTests :: Array ((Int -> Maybe (Array Int)) /\ Int /\ Array Int)
  neighborhoodTests = map (neighbours /\ _) [
    10 /\ [9, 2],
    11 /\ [10, 9, 2, 3],
    12 /\ [11, 2, 3],
    13 /\ [12, 3],
    14 /\ [13, 12, 3, 4],
    15 /\ [14, 3, 4, 5],
    16 /\ [15, 4, 5],
    17 /\ [16, 5],
    18 /\ [17, 16, 5, 6],
    19 /\ [18, 5, 6, 7],
    20 /\ [19, 6, 7],
    21 /\ [20, 7],
    22 /\ [21, 20, 7, 8],
    23 /\ [22, 7, 8, 9],
    24 /\ [23, 10, 8, 9],
    25 /\ [24, 10, 9],
    26 /\ [25, 10],
    27 /\ [26, 25, 10, 11],
    28 /\ [27, 10, 11, 12],
    29 /\ [28, 11, 12, 13],
    30 /\ [29, 12, 13],
    31 /\ [30, 13],
    32 /\ [31, 30, 13, 14],
    33 /\ [32, 13, 14, 15]
    ]
