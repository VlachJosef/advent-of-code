module Test.Year2017.Day9 where

import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (isRight)
import Data.Functor (map)
import Data.Show (show)
import Data.String (take)
import Data.Tuple.Nested (type (/\), (/\))
import Prelude (Unit, ($), (<>), (==))
import Text.Parsing.StringParser (runParser)
import Year2017.Day9 (countGarbages, countGroups, countScores, pStream)
import Year2017.Day9.Input (input)

day9Tests :: forall e. Eff (console :: CONSOLE | e) Unit
day9Tests = foreachE (tests) (\(f /\ inputData) ->
                              let
                                res = runParser f inputData
                              in do
                                log $ show (isRight res)
                                  <> " --- "
                                  <> inputData
                                  <> " should be `Right`, was "
                                  <> show res) where

  tests = map (pStream /\ _) [ "{}"
                             , "{{}}"
                             , "{{{}}}"
                             , "{{{{}}}}"
                             , "{{{{{{{}}}}}}}"
                             , "{{},{}}"
                             , "{{},{{}},{}}"
                             , "{{{},{},{{}}}}"
                             , "{<abc>}"
                             , "{{},<abc>}"
                             , "{{},<abc>,{}}"
                             , "{{},<abc>,<def>,{}}"
                             , "{{},<abc>,<def>,{{{}}}}"
                             , "{{},<abc>,<def>,{{{<ghi>,<jkl>}}}}"
                             , "{<a>,<a>,<a>,<a>}"
                             , "{{<ab>},{<ab>},{<ab>},{<ab>}}"
                             , "{{<!!>},{<!!>},{<!!>},{<!!>}}"
                             , "{{<a!>},{<a!>},{<a!>},{<ab>}}"
                             ]

day9Tests2 :: forall e. Eff (console :: CONSOLE | e) Unit
day9Tests2 = foreachE (tests) (\(f /\ inputData /\ expected) ->
                                let
                                  res = f inputData
                                in do
                                  log $ show (res == expected)
                                    <> " --- score of: "
                                    <> take 40 inputData
                                    <> " should be "
                                    <> show expected
                                    <> ", was "
                                    <> show res) where

  tests :: Array ((String -> Int) /\ String /\ Int)
  tests = map (countGroups /\ _) [ "{}"                        /\ 1
                                 , "{{{}}}"                    /\ 3
                                 , "{{},{}}"                   /\ 3
                                 , "{{{},{},{{}}}}"            /\ 6
                                 , "{<{},{},{{}}>}"            /\ 1
                                 , "{<a>,<a>,<a>,<a>}"         /\ 1
                                 , "{{<a>},{<a>},{<a>},{<a>}}" /\ 5
                                 , "{{<!>},{<!>},{<!>},{<a>}}" /\ 2
                                 , input                       /\ 2352
                                 ]

day9Tests3 :: forall e. Eff (console :: CONSOLE | e) Unit
day9Tests3 = foreachE (tests) (\(f /\ inputData /\ expected) ->
                                let
                                  res = f inputData
                                in do
                                  log $ show (res == expected)
                                    <> " --- group of: "
                                    <> take 40 inputData
                                    <> " should be "
                                    <> show expected
                                    <> ", was "
                                    <> show res) where

  tests :: Array ((String -> Int) /\ String /\ Int)
  tests = map (countScores /\ _) [ "{}"                            /\ 1
                                 , "{{{}}}"                        /\ 6
                                 , "{{},{}}"                       /\ 5
                                 , "{{{},{},{{}}}}"                /\ 16
                                 , "{<a>,<a>,<a>,<a>}"             /\ 1
                                 , "{{<ab>},{<ab>},{<ab>},{<ab>}}" /\ 9
                                 , "{{<!!>},{<!!>},{<!!>},{<!!>}}" /\ 9
                                 , "{{<a!>},{<a!>},{<a!>},{<ab>}}" /\ 3
                                 , input                           /\ 21037
                                 ]

day9Tests4 :: forall e. Eff (console :: CONSOLE | e) Unit
day9Tests4 = foreachE (tests) (\(f /\ inputData /\ expectedLenght) ->
                                let
                                  res = f inputData
                                in do
                                  log $ show (res == expectedLenght)
                                    <> " --- garbage of: "
                                    <> take 40 inputData
                                    <> " should be "
                                    <> show expectedLenght
                                    <> ", was "
                                    <> show res) where

  tests :: Array ((String -> Int) /\ String /\ Int)
  tests = map (countGarbages /\ _) [ "<>"                  /\ 0
                                   , "<random characters>" /\ 17
                                   , "<<<<>"               /\ 3
                                   , "<{!>}>"              /\ 2
                                   , "<!!>"                /\ 0
                                   , "<!!!>>"              /\ 0
                                   , "<{o\"i!a,<{i<a>"     /\ 10
                                   , input                 /\ 9495
                                   ]
