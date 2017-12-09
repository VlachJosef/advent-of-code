module Test.Main where

import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE)
import Prelude (Unit, id)
import Test.Year2017.Day1 (day1Tests)
import Test.Year2017.Day2 (day2Tests)
import Test.Year2017.Day3 (day3Tests, day3NeighboursTests)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = foreachE [day1Tests, day2Tests, day3Tests, day3NeighboursTests] id
