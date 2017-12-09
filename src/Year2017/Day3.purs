module Year2017.Day3 where

-- http://adventofcode.com/2017/day/3

import Data.Array (cons, elemIndex, filter, index, last, length, mapWithIndex, snoc, (..))
import Data.Foldable (foldl)
import Data.Functor (map)
import Data.Int as Int
import Data.Maybe (Maybe, fromMaybe)
import Data.Traversable (sequence)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Math (sqrt)
import Prelude (bind, discard, max, mod, pure, ($), (&&), (+), (-), (/), (==), (*), (<>), (>=), (<), (>), (<=))

type Index = Int
type Sum = Int

initial :: Array Int
initial = [1, 1, 2, 4, 5, 10, 11, 23, 25]

initialWithIndex :: Array (Int /\ Int)
initialWithIndex = mapWithIndex (\i a -> (i + 1) /\ a) initial

getBase :: Int -> Int
getBase sideArray = base where
  baseX    = Int.ceil $ sqrt $ Int.toNumber sideArray
  reminder = baseX `mod` 2
  base    = baseX + 1 - reminder

precalculateData :: Index -> Array Index /\ Array Index /\ Array Index
precalculateData n = sideArray /\ sideArrayInnerExt /\ (predecessor <> squareBeginning) where
  base                    = getBase n
  baseInner               = base - 2
  baseSquared             = base * base
  baseInnerSquared        = baseInner * baseInner
  squareSideLength        = (baseSquared - baseInnerSquared) / 4
  side                    = (baseSquared - n) / squareSideLength
  followingCorner /\
  sideBeginning /\
  sideArray               = sideInfo base side
  followingCornerInner /\
  sideBeginningInner /\
  sideArrayInner          = sideInfo baseInner side
  first                   = if side == 3 then 0 else sideBeginningInner - 1
  squareBeginning         = if side == 0 && followingCorner - n <= 1 then [followingCornerInner + 1] else []
  sideArrayInnerExt       = snoc (cons first sideArrayInner) 0
  predecessor             = if n == sideBeginning then
                              case side of
                                3 -> [n-1]
                                otherwise -> [n-1, n-2]
                            else if n == sideBeginning + 1 then
                              case side of
                                3 -> [n-1, n-2]
                                otherwise -> [n-1]
                            else [n-1]

sideInfo :: Int -> Int -> Int /\ Int /\ Array Int
sideInfo base side = followingCorner /\ sideBeginning /\ sideArray where
  followingCorner = corner base side
  sideBeginning   = followingCorner + 2 - base
  sideArray       = sideBeginning .. followingCorner

-- 65  64  63  62  61  60  59  58  57
-- 66  37  36  35  34  33  32  31  56
-- 67  38  17  16  15  14  13  30  55
-- 68  39  18   5   4   3  12  29  54
-- 69  40  19   6   1   2  11  28  53
-- 70  41  20   7   8   9  10  27  52
-- 71  42  21  22  23  24  25  26  51
-- 72  43  44  45  46  47  48  49  50
-- 73  74  75  76  77  78  79  80  81  82
--                                    121
innerNeighbours :: Index -> Array Index -> Array Index -> Maybe (Array Index)
innerNeighbours idx sideArray sideArrayInner = do
  elemIdx      <- elemIndex idx sideArray
  indexes      <- pure $ filter (\a -> a >= 0 && a < length sideArray) [elemIdx - 1, elemIdx, elemIdx + 1]
  indexesInner <- sequence $ map (index sideArrayInner) indexes
  pure $ filter (_ > 0) indexesInner

neighbours :: Index -> Maybe (Array Index)
neighbours idx = map (predecessor <> _) $ innerNeighbours idx sideArray sideArrayInner where
  sideArray /\ sideArrayInner /\ predecessor = precalculateData idx

getSumAtIndex :: Array Sum -> Index -> Maybe (Index /\ Sum)
getSumAtIndex sums idx = do
    idxs <- neighbours idx
    sums <- sequence $ map (\a -> index sums (a - 1)) idxs
    pure $ idx /\ foldl (+) 0 sums

getNextSum :: Array (Index /\ Sum) -> Maybe (Index /\ Sum)
getNextSum xs = do
  idx /\ sum <- last xs
  getSumAtIndex (map snd xs) $ idx + 1

getBigger :: Int -> Maybe Sum
getBigger n = getBigger' initialWithIndex
  where
    getBigger' arr =
      do
        idx /\ sum <- getNextSum arr
        if sum > n then pure sum else getBigger' (snoc arr (idx /\ sum))

corner :: Int -> Int -> Int
corner base n = base * base - n * (base - 1)

solution1:: Int -> Int
solution1 1 = 0
solution1 sideArray = max sub (base - sub) where
  baseX    = Int.ceil $ sqrt $ Int.toNumber sideArray
  reminder = baseX `mod` 2
  base0    = baseX - 1 - reminder
  y        = sideArray - (base0 * base0)
  base     = base0 + 1
  sub      = mod y base

solution2 :: Int -> Int
solution2 n = fromMaybe 0 (getBigger n)
