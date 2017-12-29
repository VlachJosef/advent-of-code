module Year2017.Day15 where

-- http://adventofcode.com/2017/day/15

import Data.Int (floor, pow, toNumber)
import Data.Int.Bits (and)
import Data.Tuple.Nested (type (/\), (/\))
import Prelude (($), (*), (+), (-), (/), (<=), (==))

type GenValue = Int -> Number -> Int /\ Int

factorA :: Number
factorA = 16807.0

factorB :: Number
factorB = 48271.0

divider :: Number
divider = 2147483647.0

mask :: Int
mask = (pow 2 16) - 1

nextValue :: GenValue
nextValue a factor  = let
  veryBigNumber = toNumber a * factor
  multiplier    = toNumber $ floor $ veryBigNumber / divider
  next          = floor (veryBigNumber - (multiplier * divider))
  next16        = and mask next
  in next /\ next16

nextValueDiv :: Int -> GenValue
nextValueDiv divisibleBy a factor  = let
  next /\ next16 = nextValue a factor
  in if and next (divisibleBy - 1) == 0
     then next /\ next16
     else nextValueDiv divisibleBy next factor

genGen :: GenValue -> GenValue -> Int -> Int -> Int -> Int
genGen fa fb inputA inputB iterationCount = go iterationCount inputA inputB 0 where
  go :: Int -> Int -> Int -> Int -> Int
  go iterationCount a b matches =
    if iterationCount <= 0
    then matches
    else let
      nextA /\ nextA16 = fa a factorA
      nextB /\ nextB16 = fb b factorB
      match = if nextA16 == nextB16 then 1 else 0
      in go (iterationCount - 1) nextA nextB (matches + match)

solution1 :: Int -> Int -> Int -> Int
solution1 = genGen nextValue nextValue

solution2 :: Int -> Int -> Int -> Int
solution2 = genGen (nextValueDiv 4) (nextValueDiv 8)
