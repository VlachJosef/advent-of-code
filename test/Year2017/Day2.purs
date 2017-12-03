module Test.Year2017.Day2 where

import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Data.Tuple.Nested (type (/\), (/\))
import Prelude (Unit, (==), (<>), ($))
import Year2017.Day2 (SpreadSheet, input, solution1, solution2)

day2Tests :: forall e. Eff (console :: CONSOLE | e) Unit
day2Tests = foreachE (solution1Tests <> solution2Tests) (\(f /\ a /\ b) -> do
                                            log $ show (f a == Just b) <> " --- " <> show a <> " should be " <> show b) where
  solution1Tests :: Array ((SpreadSheet -> Maybe Int) /\ SpreadSheet /\ Int)
  solution1Tests = map (solution1 /\ _)[
    solution1TestInput  /\ 18,
    input               /\ 42299
  ]

  solution2Tests :: Array ((SpreadSheet -> Maybe Int) /\ SpreadSheet /\ Int)
  solution2Tests = map (solution2 /\ _)[
    solution2TestInput /\ 9,
    input              /\ 277]

solution1TestInput :: SpreadSheet
solution1TestInput = [
  [5, 1, 9, 5],
  [7, 5, 3   ],
  [2, 4, 6, 8]
]

solution2TestInput :: SpreadSheet
solution2TestInput = [
  [5, 9, 2, 8],
  [9, 4, 7, 3],
  [3, 8, 6, 5]
]
