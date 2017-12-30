module Year2017.Day17 where

-- http://adventofcode.com/2017/day/17

import Control.Applicative (pure)
import Data.Array (foldRecM, index, insertAt, length, singleton, (..))
import Data.Field (mod)
import Data.Foldable (foldl)
import Data.Functor (map)
import Data.Maybe (Maybe)
import Data.Newtype (wrap)
import Prelude (bind, ($), (+), (==))
import Year2017.Day17.Types (CurrentPosition(..), State(..), StateLike(..), NextElement(..), NumberOfSteps(..))

initial :: State Int
initial = State { currentPosition: CurrentPosition 0
                , array: singleton 0
                }

initialLike :: StateLike
initialLike = StateLike { currentPosition: CurrentPosition 0
                        , nextToZero: 0
                        }

solution1 :: NumberOfSteps -> Maybe Int
solution1 nos = do
  State { array, currentPosition: CurrentPosition cp } <- foldRecM (insertNext nos) initial (map wrap $ 1 .. 2017)
  index array cp

solution2 :: Int -> NumberOfSteps -> Int
solution2 iter nos = let
  StateLike res = foldl (insertNextLike nos) initialLike (map wrap $ 1 .. iter)
  in res.nextToZero

insertNextLike :: NumberOfSteps -> StateLike -> NextElement -> StateLike
insertNextLike nos (s @ StateLike { nextToZero }) (NextElement ne) = let
  idx = nextInsertionIndexLike s ne nos
  nextToZeroUpd = if idx == 0 then ne else nextToZero
  in wrap { currentPosition: CurrentPosition $ idx + 1
          , nextToZero: nextToZeroUpd
          }

insertNext :: NumberOfSteps -> State Int -> NextElement -> Maybe (State Int)
insertNext nos (s @ State { array }) (NextElement ne) = let
  idx = nextInsertionIndex s nos
  in do
    arrUpd <- insertAt idx ne array
    pure $ wrap { currentPosition: CurrentPosition $ idx + 1
                , array: arrUpd
                }

nextInsertionIndex :: forall a. State a -> NumberOfSteps -> Int
nextInsertionIndex (State { currentPosition, array }) = insertionIndex currentPosition (length array)

nextInsertionIndexLike :: StateLike -> Int -> NumberOfSteps -> Int
nextInsertionIndexLike (StateLike { currentPosition }) = insertionIndex currentPosition

insertionIndex :: CurrentPosition -> Int -> NumberOfSteps -> Int
insertionIndex (CurrentPosition from) arrayLen (NumberOfSteps numberOfSteps) = (from + numberOfSteps) `mod` arrayLen
