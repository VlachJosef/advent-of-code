module Test.Year2017.Day4 where

import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array (take)
import Data.Functor (map)
import Data.Show (show)
import Data.Tuple.Nested (type (/\), (/\))
import Prelude (Unit, (==), (<>), ($))
import Year2017.Day4 (Data, input, solution1, solution2)

day4Tests :: forall e. Eff (console :: CONSOLE | e) Unit
day4Tests = foreachE (solution1Tests <> solution2Tests) (\(f /\ testee /\ expected) -> do
                                                            log $ show (f testee == expected) <> " --- " <> show (take 10 testee) <> " should be " <> show expected <> ", was " <> show (f testee)) where
  solution1Tests :: Array ((Data -> Int) /\ Data /\ Int)
  solution1Tests = map (solution1 /\ _) [
    [
      ["aa", "bb", "cc", "dd", "ee"],
      ["aa", "bb", "cc", "dd", "aa"],
      ["aa", "bb", "cc", "dd", "aaa"]] /\ 2,
    input /\ 477]

  solution2Tests :: Array ((Data -> Int) /\ Data /\ Int)
  solution2Tests = map (solution2 /\ _) [
    [
      ["abcde", "fghij"],
      ["abcde", "xyz", "ecdab"],
      ["a", "ab", "abc", "abd", "abf", "abj"],
      ["iiii", "oiii", "ooii", "oooi", "oooo"],
      ["oiii", "ioii", "iioi", "iiio"]
    ] /\ 3,
    input  /\ 167
  ]
