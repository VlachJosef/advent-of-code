module Test.Year2017.Day7 where

import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.Semigroup (append)
import Data.Show (show)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unit (unit)
import Prelude (Unit, id, ($), (<>), (==))
import Year2017.Day7 (Answer(..), TreeData, Name, solution)
import Year2017.Day7.Input (input, inputTest, inputTestBalanced)

day7Tests :: forall e. Eff (console :: CONSOLE | e) Unit
day7Tests = foreachE (solution1Tests) (\(f /\ inputData /\ expected) -> do
                                          log $ show (f inputData == expected)
                                            <> " --- "
                                            <> " should be "
                                            <> show expected
                                            <> ", was "
                                            <> show (f inputData)) where

   solution1Tests :: Array ((Array TreeData -> Maybe (Name /\ Int)) /\ (Array TreeData) /\ (Maybe (Name /\ Int)))
   solution1Tests = map (solution /\ _) [ inputTest         /\ Just ("ugml"  /\ 60)
                                        , input             /\ Just ("obxrn" /\ 749)
                                        , inputTestBalanced /\ Nothing
                                        ]

day7Tests2 :: forall e. Eff (console :: CONSOLE | e) Unit
day7Tests2 = foreachE (solution2Tests) (\(f /\ input1 /\ input2 /\ expected) -> do
                                          log $
                                            show (f input1 input2 == expected)
                                            <> " --- "
                                            <> show input1
                                            <> " append "
                                            <> show input2
                                            <> " should be "
                                            <> show expected
                                            <> ", was "
                                            <> show (f input1 input2)) where


   solution2Tests :: Array ((Answer -> Answer -> Answer) /\ Answer /\ Answer /\ Answer)
   solution2Tests = map (append /\ _) [ l1 /\ l2 /\ l1
                                      , l2 /\ l1 /\ l1
                                      , l1 /\ r1 /\ r1
                                      , r1 /\ l1 /\ r1
                                      , r1 /\ r2 /\ r12
                                      ]

day7Tests3 :: forall e. Eff (console :: CONSOLE | e) Unit
day7Tests3 = foreachE (solution3Tests) (\(f /\ input1 /\ input2 /\ expected) -> do
                                          log $
                                            show (f id [input1, input2] == expected)
                                            <> " --- "
                                            <> show input1
                                            <> " append "
                                            <> show input2
                                            <> " should be "
                                            <> show expected
                                            <> ", was "
                                            <> show (f id [input1, input2])) where

   solution3Tests :: Array (((Answer -> Answer) -> (Array Answer) -> Answer) /\ Answer /\ Answer /\ Answer)
   solution3Tests = map (foldMap /\ _) [ l1 /\ l2 /\ l1
                                       , l2 /\ l1 /\ l1
                                       , l1 /\ r1 /\ r1
                                       , r1 /\ l1 /\ r1
                                       , r1 /\ r2 /\ r12
                                       ]

l1 :: Answer
l1 = Answer $ Left unit

l2 :: Answer
l2 = Answer $ Left unit

r1 :: Answer
r1 = Answer $ Right [("A" /\ 1 /\ [])]

r2 :: Answer
r2 = Answer $ Right [("B" /\ 2 /\ [])]

r12 :: Answer
r12 = Answer $ Right [("A" /\ 1 /\ []), ("B" /\ 2 /\ [])]
