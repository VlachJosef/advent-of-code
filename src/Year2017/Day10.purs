module Year2017.Day10 where

-- http://adventofcode.com/2017/day/10

import Data.Array (drop, length, reverse, slice, snoc, take, (..))
import Data.Char as C
import Data.EuclideanRing (mod)
import Data.Foldable (foldl, product)
import Data.Functor (map)
import Data.Int (hexadecimal, toStringAs)
import Data.Int.Bits (xor)
import Data.Monoid (mempty)
import Data.Newtype (over, unwrap, wrap)
import Data.Show (show)
import Data.String (joinWith)
import Data.String as S
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Prelude (($), (+), (<<<), (<>), (==), (>))
import Year2017.Day10.Input (input)
import Year2017.Day10.Types (CurrentPosition(CurrentPosition), DenseHash, Input(Input), Length(Length), Lengths(Lengths), Skip(Skip))

lengthsAsString :: Lengths -> String
lengthsAsString (Lengths xs) = joinWith "," (map (show <<< unwrap) xs)

suffixLengths :: Array Int
suffixLengths = [17, 31, 73, 47, 23]

runRounds :: Input -> Lengths -> DenseHash
runRounds i (Lengths l) = let
  sparseHash /\ _ /\ _ = foldl runRound (i /\ mempty /\ mempty) (1 .. 64)
  in toDenseHash sparseHash where

    runRound :: (Input /\ CurrentPosition /\ Skip) -> Int -> (Input /\ CurrentPosition /\ Skip)
    runRound acc _ = foldl applyStep acc l

increase :: Skip -> Skip
increase skip = over Skip (_ + 1) skip

nextCp :: CurrentPosition -> Length -> Skip -> Input -> CurrentPosition
nextCp (CurrentPosition cp) (Length len) (Skip skip) xs = CurrentPosition ((cp + len + skip) `mod` length (unwrap xs))

applyStep :: (Input /\ CurrentPosition /\ Skip) -> Length -> (Input /\ CurrentPosition /\ Skip)
applyStep (xs /\ cp /\ skip) h = let
  xsNext   = step xs cp h
  cpNext   = nextCp cp h skip xs
  skipNext = increase skip
  in xsNext /\ cpNext /\ skipNext

solution2 :: String -> DenseHash
solution2 inputString = let
  lenghts = finalLenghts inputString
  in runRounds input lenghts

denseHash :: Input -> Array Int
denseHash (Input xs) = go xs [] where
  go [] acc = acc
  go xs acc = let
    h /\ t = splitAt 16 xs
    xored  = xorSegment h
    in go t $ snoc acc xored

xorSegment :: Array Int -> Int
xorSegment = foldl (xor) 0

toDenseHash :: Input -> DenseHash
toDenseHash = wrap <<< foldl (<>) "" <<< map (padding <<< toStringAs hexadecimal) <<< denseHash

padding :: String -> String
padding s = if S.length s == 1 then "0" <> s else s

finalLenghts :: String -> Lengths
finalLenghts inputString = let
  input = map (C.toCharCode) $ S.toCharArray inputString
  in Lengths $ map Length (input <> suffixLengths)

solution :: Input -> Lengths -> Int
solution inputs lenghts = let
  res         = foldl applyStep (inputs /\ mempty /\ mempty) (unwrap lenghts)
  getSolution = product <<< take 2 <<< unwrap <<< fst
  in getSolution res

step :: Input -> CurrentPosition -> Length -> Input
step (Input i) (CurrentPosition cp) (Length l) = let
  overflow = cp + l
  li       = length i
  in if overflow > li then
       let
         m                 = overflow `mod` li
         beg /\ mid /\ end = splitAt2 m cp i
         endLen            = length end
         reversed          = reverse (end <> beg)
         s /\ e            = splitAt endLen reversed
       in Input $ e <> mid <> s
     else
       let
         s /\ m /\ e = splitAt2 cp overflow i
         reversed    = reverse m
       in Input $ s <> reversed <> e


splitAt :: forall len. Int -> Array len -> Array len /\ Array len
splitAt s xs = let
  start = take s xs
  end   = drop s xs
  in start /\ end

splitAt2 :: forall len. Int -> Int -> Array len -> Array len /\ Array len /\ Array len
splitAt2 from to xs = let
  start   = take from xs
  subList = slice from to xs
  end     = drop to xs
  in start /\ subList /\ end
