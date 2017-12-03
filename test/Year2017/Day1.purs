module Test.Year2017.Day1 where

import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Data.Tuple.Nested (type (/\), (/\))
import Prelude (Unit, (==), (<>), ($))
import Year2017.Day1 (input, solution1, solution2)

day1Tests :: forall e. Eff (console :: CONSOLE | e) Unit
day1Tests = foreachE (solution1Tests <> solution2Tests) (\(f /\ a /\ b) -> do
                                            log $ show (f a == Just b) <> " --- " <> a <> " should be " <> (show b)) where
  solution1Tests :: Array ((String -> Maybe Int) /\ String /\ Int)
  solution1Tests = map (solution1 /\ _)[
    "1122"     /\ 3,
    "1111"     /\ 4,
    "1234"     /\ 0,
    "91212129" /\ 9,
    input      /\ 1341]

  solution2Tests :: Array ((String -> Maybe Int) /\ String /\ Int)
  solution2Tests = map (solution2 /\ _)[
    "1212"     /\ 6,
    "1221"     /\ 0,
    "123425"   /\ 4,
    "123123"   /\ 12,
    "12131415" /\ 4,
    input      /\ 1348]
