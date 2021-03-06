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
import Test.Year2017.Day9 (day9Tests, day9Tests2, day9Tests3, day9Tests4)
import Test.Year2017.Day10 (day10Tests, day10Tests2, day10Tests3)
import Test.Year2017.Day11 (day11Tests, day11Tests2, day11Tests3)
import Test.Year2017.Day12 (day12Tests, day12Tests2)
import Test.Year2017.Day13 (day13Tests, day13Tests2)
import Test.Year2017.Day14 (day14Tests, day14Tests2)
import Test.Year2017.Day15 (day15Tests, day15Tests2)
import Test.Year2017.Day16 (day16Tests, day16Tests2)
import Test.Year2017.Day17 (day17Tests, day17Tests2)
import Test.Year2017.Day18 (day18Tests, day18Tests2)
import Test.Year2017.Day19 (day19Tests, day19Tests2)
import Test.Year2017.Day20 (day20Tests, day20Tests2)
import Test.Year2017.Day21 (day21Tests, day21Tests2, day21Tests3, day21Tests4, day21Tests5, day21Tests6, day21Tests7)
import Test.Year2017.Day22 (day22Tests, day22Tests2)


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
                , day9Tests
                , day9Tests2
                , day9Tests3
                , day9Tests4
                , day10Tests
                , day10Tests2
                , day10Tests3
                , day11Tests
                , day11Tests2
                , day11Tests3
                , day12Tests
                , day12Tests2
                , day13Tests
                , day13Tests2
                , day14Tests
                , day14Tests2
                , day15Tests
                , day15Tests2
                , day16Tests
                , day16Tests2
                , day17Tests
                , day17Tests2
                , day18Tests
                , day18Tests2
                , day19Tests
                , day19Tests2
                , day20Tests
                , day20Tests2
                , day21Tests
                , day21Tests2
                , day21Tests3
                , day21Tests4
                , day21Tests5
                , day21Tests6
                , day21Tests7
                , day22Tests
                , day22Tests2
                ] id
