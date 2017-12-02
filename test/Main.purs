module Test.Main where

import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Data.Tuple.Nested ((/\))
import Prelude (Unit, (==), (<>), ($))
import Year2017.Day1 (input, solution1, solution2)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = foreachE (tests1 <> tests2) (\(f /\ a /\ b) -> do
           log $ show (f a == Just b) <> " --- " <> a <> " should be " <> (show b)) where
  tests1 = map (solution1 /\ _)[
    "1122"     /\ 3,
    "1111"     /\ 4,
    "1234"     /\ 0,
    "91212129" /\ 9,
    input      /\ 1341]

  tests2 = map (solution2 /\ _)[
    "1212"     /\ 6,
    "1221"     /\ 0,
    "123425"   /\ 4,
    "123123"   /\ 12,
    "12131415" /\ 4,
    input      /\ 1348]
