module Test.Year2017.Day8 where

import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Data.Tuple.Nested (type (/\), (/\))
import Prelude (Unit, ($), (<>), (==))
import Year2017.Day8 (Instructions, solution1, solution2)
import Year2017.Day8.Input (input, inputTest)

day8Tests :: forall e. Eff (console :: CONSOLE | e) Unit
day8Tests = foreachE (solution1Tests <> solution2Tests) (\(f /\ inputData /\ expected) -> do
                                          log $ show (f inputData == Just expected)
                                            <> " --- "
                                            <> " should be "
                                            <> show expected
                                            <> ", was "
                                            <> show (f inputData)) where

  solution1Tests :: Array ((Instructions -> Maybe Int) /\ Instructions /\ Int)
  solution1Tests = map (solution1 /\ _) [ inputTest /\ 1
                                        , input     /\ 4163
                                        ]

  solution2Tests :: Array ((Instructions -> Maybe Int) /\ Instructions /\ Int)
  solution2Tests = map (solution2 /\ _) [ inputTest /\ 10
                                        , input     /\ 5347
                                        ]
