module Test.Main where

import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE)
import Prelude (Unit, id)
import Test.Year2017.Day1 (day1Tests)
import Test.Year2017.Day2 (day2Tests)
import Test.Year2017.Day3 (day3Tests, day3NeighboursTests)
import Test.Year2017.Day4 (day4Tests)
import Test.Year2017.Day5 (day5Tests)
import Test.Year2017.Day6 (day6Tests, day6Tests2)
import Test.Year2017.Day7 (day7Tests, day7Tests2, day7Tests3)
import Test.Year2017.Day8 (day8Tests)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = foreachE [ day1Tests
                , day2Tests
                , day3Tests
                , day3NeighboursTests
                , day4Tests
                , day5Tests
                , day6Tests
                , day6Tests2
                , day7Tests
                , day7Tests
                , day7Tests2
                , day7Tests3
                , day8Tests
                ] id
