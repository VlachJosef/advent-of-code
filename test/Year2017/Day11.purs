module Test.Year2017.Day11 where

import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array (take)
import Data.Functor (map)
import Data.Int (fromNumber, round)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap, wrap)
import Data.Show (show)
import Data.Tuple.Nested ((/\))
import Prelude (Unit, negate, ($), (<>), (==))
import Year2017.Day11 (distance, furthest, walk, walkDistance)
import Year2017.Day11.Input (input)
import Year2017.Day11.Types (Move(..), Node(..))

day11Tests :: forall e. Eff (console :: CONSOLE | e) Unit
day11Tests = foreachE (tests) (\(f /\ steps /\ expected) ->
                              let
                                res = f steps
                              in do
                                log $ show (res == expected)
                                  <> " --- walkDistance "
                                  <> show (take 10 steps)
                                  <> " res: "
                                  <> show res
                                  <> " should be: "
                                  <> show expected
                                  <> ", was "
                                  <> show res) where

  tests = map (walkDistance /\ _) [ [NE, SW]             /\ 0
                                  , [NE, NE, NE]         /\ 3
                                  , [NE, NE, SW, SW]     /\ 0
                                  , [NE, NE, S, S]       /\ 2
                                  , [SE, SW, SE, SW, SW] /\ 3
                                  , [NE, NE, NE, N, N]   /\ 5
                                  , input                /\ 824
                                  ]

day11Tests3 :: forall e. Eff (console :: CONSOLE | e) Unit
day11Tests3 = foreachE (tests) (\(f /\ steps /\ expected) ->
                              let
                                res = f steps
                              in do
                                log $ show (res == Just expected)
                                  <> " --- furthest "
                                  <> show (take 10 steps)
                                  <> " res: "
                                  <> show res
                                  <> " should be: "
                                  <> show expected
                                  <> ", was "
                                  <> show res) where

  tests = map (furthest /\ _) [ [NE, SW]             /\ 1
                              , [NE, NE, NE]         /\ 3
                              , [NE, NE, SW, SW]     /\ 2
                              , [NE, NE, S, S]       /\ 2
                              , [SE, SW, SE, SW, SW] /\ 3
                              , [NE, NE, NE, N, N]   /\ 5
                              , input                /\ 1548
                              ]

day11Tests2 :: forall e. Eff (console :: CONSOLE | e) Unit
day11Tests2 = foreachE (tests) (\(f /\ steps /\ expected) ->
                              let
                                res = f steps
                              in do
                                log $ show (res == expected)
                                  <> " --- walk "
                                  <> show (take 10 steps)
                                  <> " should be: "
                                  <> show expected
                                  <> ", was "
                                  <> show res) where

  tests = map (walk /\ _) [ []                               /\ Node 0 0
                          , [NW]                             /\ Node (-1) (-1)
                          , [N]                              /\ Node (-2) 0
                          , [NE]                             /\ Node (-1) 0
                          , [SW]                             /\ Node 1 (-1)
                          , [S]                              /\ Node 2 0
                          , [SE]                             /\ Node 1 0
                          , [NW, NW]                         /\ Node (-2) (-1)
                          , [N, N]                           /\ Node (-4) 0
                          , [NE, NE]                         /\ Node (-2) 1
                          , [SW, SW]                         /\ Node 2 (-1)
                          , [S, S]                           /\ Node 4 0
                          , [SE, SE]                         /\ Node 2 1
                          , [N, SE, S, SW, NW, N, NE, S]     /\ Node 0 0
                          , [NE, NE, NE, NE, SW, SW, SW, SW] /\ Node 0 0
                          , [NE, NE, NE, N, N]               /\ Node (-7) 1
                          , input                            /\ Node 1211 (-219)
                          ]
