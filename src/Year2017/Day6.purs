module Year2017.Day6 where

-- http://adventofcode.com/2017/day/6

import Control.Applicative (pure)
import Control.Monad.Rec.Class (Step(Loop, Done), tailRecM)
import Data.Array (cons, elemIndex, length, replicate, updateAt, zipWith)
import Data.Foldable (maximum)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Prelude (bind, mod, ($), (+), (-), (/), (<=), (<>))

type Index       = Int
type Residue     = Int
type Divider     = Int
type Length      = Int
type MemoryState = Array Int
type LoopData    = { idx          :: Index
                   , residue      :: Residue
                   , divider      :: Divider
                   , memory       :: MemoryState
                   , memoryStates :: Array MemoryState
                   }

solution :: MemoryState -> Maybe (Int /\ Int)
solution memory = do
  idx /\ residue /\ divider <- nextData memory
  distributeTailRecM idx residue divider memory

nextData :: MemoryState -> Maybe (Index /\ Residue /\ Divider)
nextData memory = let len = length memory in do
  mostBlocks <- maximum memory
  idx        <- elemIndex mostBlocks memory
  pure $ idx /\ (mostBlocks `mod` len) /\ (mostBlocks / len)

distributeTailRecM :: Index -> Residue -> Divider -> MemoryState -> Maybe (Int /\ Int)
distributeTailRecM idx residue divider memory = tailRecM go {idx, residue, divider, memory, memoryStates: pure memory} where
  go :: LoopData -> Maybe (Step LoopData (Int /\ Int))
  go {idx, residue, divider, memory, memoryStates: acc} = let
    len        = length memory
    dividerArr = replicate len divider
    residueArr = redistributeBlocks (idx + 1) residue len
    increments = zipWith (+) dividerArr residueArr
    in do
      xsIndexReset                       <- updateAt idx 0 memory
      memoryUpd                          <- pure $ zipWith (+) xsIndexReset increments
      idxUpd /\ residueUpd /\ dividerUpd <- nextData memoryUpd
      pure $ case elemIndex memoryUpd acc of
        Just loopSize  -> Done $ (loopSize + 1) /\ (length acc)
        Nothing        -> Loop { idx          : idxUpd
                               , residue      : residueUpd
                               , divider      : dividerUpd
                               , memory       : memoryUpd
                               , memoryStates : (cons memoryUpd acc)
                               }

redistributeBlocks :: Index -> Residue -> Length -> MemoryState
redistributeBlocks idx residue len =
  if idx + residue <= len then
    replicate idx     0 <>
    replicate residue 1 <>
    replicate (len - idx - residue) 0
  else
    replicate overflow         1 <>
    replicate (idx - overflow) 0 <>
    replicate (len - idx) 1 where
      overflow = (idx + residue) `mod` len

inputTest :: MemoryState
inputTest = [0, 2, 7, 0]

input :: MemoryState
input = [4, 1, 15, 12, 0, 9, 9, 5, 5, 8, 7, 3, 14, 5, 12, 3]
