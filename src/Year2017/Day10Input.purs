module Year2017.Day10.Input where

-- http://adventofcode.com/2017/day/10

import Data.Array ((..))
import Data.Functor (map)
import Prelude (($))
import Year2017.Day10.Types (Input(..), Length(..), Lengths(..))

testInput :: Input
testInput = Input $ 0 .. 4

testLengths :: Lengths
testLengths = Lengths $ map Length [3, 4, 1, 5]

input :: Input
input = Input $ 0 .. 255

lengths :: Lengths
lengths = Lengths $ map Length [83, 0, 193, 1, 254, 237, 187, 40, 88, 27, 2, 255, 149, 29, 42, 100]
