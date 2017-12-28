module Year2017.Day14 where

-- http://adventofcode.com/2017/day/14

import Control.Applicative (pure)
import Control.Monad.Eff (Eff)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.ST (ST, pureST)
import Data.Array as A
import Data.Array.ST (STArray, peekSTArray, pokeSTArray, thaw, unsafeFreeze)
import Data.Foldable (foldMap, foldl)
import Data.Function (id)
import Data.Functor (map)
import Data.Int (binary, fromStringAs, hexadecimal, toStringAs)
import Data.Maybe (Maybe(Just), maybe)
import Data.Show (show)
import Data.String (Pattern(..), Replacement(..), drop, fromCharArray, length, replaceAll, take, toCharArray)
import Data.Traversable (sequence)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Prelude (bind, ($), (&&), (+), (-), (<), (>), (<<<), (<>), (>=), (>>=))
import Year2017.Day10 as Day10
import Year2017.Day10.Types (DenseHash(DenseHash))
import Year2017.Day14.Types (XO(..))

toXO :: String -> Array XO
toXO str = map replaceXO $ toCharArray str where

  replaceXO :: Char -> XO
  replaceXO '0' = O
  replaceXO _   = X

solution1 :: String -> Int
solution1 str = let

  xs :: Array (Maybe String)
  xs = map (\i -> toBinaryString (str <> "-" <> show i)) (0 A... 127)
  oneBigString = foldMap (maybe "" id) (xs)
  result = replaceAll (Pattern "0") (Replacement "") oneBigString
  in length result

makeMutable :: forall a h r. Array (Array a) -> Eff (st :: ST h | r) (STArray h (STArray h a))
makeMutable xs = (sequence $ map thaw xs) >>= thaw

peekSTArrayM :: forall a h r. STArray h a -> Int -> MaybeT (Eff (st :: ST h | r)) a
peekSTArrayM stArr = MaybeT <<< peekSTArray stArr

pokeSTArrayM :: forall a h r. STArray h a -> Int -> a -> MaybeT (Eff (st :: ST h | r)) Boolean
pokeSTArrayM stArr i = liftToMaybeT <<< pokeSTArray stArr i

liftToMaybeT :: forall a h r. Eff (st :: ST h | r) a -> MaybeT (Eff (st :: ST h | r)) a
liftToMaybeT = MaybeT <<< map Just

mark :: Array (Array XO) -> Maybe ((Array (Array XO)) /\ Int)
mark xxs = let

  expandGroup :: forall r h. Int -> STArray h (STArray h XO) -> Int /\ Int -> MaybeT (Eff ( st :: ST h | r)) Boolean
  expandGroup groupNumber xos pos@(x /\ y) = do
    row <- peekSTArrayM xos x
    xo  <- peekSTArrayM row y
    case xo of
      X -> do
        updated <- pokeSTArrayM row y (N groupNumber)
        _       <- sequence $ map (expandGroup groupNumber xos) (getNeighbors pos)
        pure updated
      _ -> MaybeT (pure (Just false))

  nextGroupNumber :: Boolean -> Int -> Int
  nextGroupNumber marked groupNumber = if marked then (groupNumber + 1) else groupNumber

  markGroup :: forall h r. Int -> Int -> Int -> STArray h (STArray h XO) -> MaybeT (Eff (st :: ST h | r)) Int
  markGroup x y groupNumber xos = do
    marked <- expandGroup groupNumber xos (x /\ y)
    if x < 127
      then markGroup (x + 1) y (nextGroupNumber marked groupNumber) xos
      else
      if y < 127
        then markGroup 0 (y + 1) (nextGroupNumber marked groupNumber) xos
        else MaybeT $ pure (Just groupNumber)

  markGroups :: forall h r. MaybeT (Eff ( st :: ST h | r)) ((Array (Array XO)) /\ Int)
  markGroups = do
     mutableXxs  <- liftToMaybeT (makeMutable xxs)
     groupNumber <- markGroup 0 0 0 mutableXxs
     freezed     <- liftToMaybeT (unsafeFreeze mutableXxs)
     liftToMaybeT (map (_ /\ groupNumber) (sequence (map unsafeFreeze freezed)))

  in pureST (runMaybeT markGroups)

getNeighbors :: Int /\ Int -> Array (Int /\ Int)
getNeighbors (x /\ y) = A.filter (\(a /\ b) -> (a >= 0 && a < 128) && (b >= 0 && b < 128))
                        [ (x + 1) /\ y
                        , (x - 1) /\ y
                        , x /\ (y + 1)
                        , x /\ (y - 1)
                        ]

solution2 :: String -> Maybe Int
solution2 str = do
    xs <- sequence $ map (\i -> toBinaryString (str <> "-" <> show i)) (0 A... 127)
    map snd $ mark $ map toXO xs

toBinaryString :: String -> Maybe String
toBinaryString str = let
  (DenseHash dh) = Day10.solution2 str

  go :: String -> Array (Maybe String) -> Maybe String
  go "" acc = map (foldl (<>) "") (sequence $ A.reverse acc)
  go s acc = let
    chunk     = take 2 s
    rest      = drop 2 s
    binString = fromStringAs hexadecimal chunk >>= pure <<< padding <<< toStringAs binary
    in go rest (A.cons binString acc)
  in go dh []

padding :: String -> String
padding s = let
  neededZeros = 8 - length s
  in if neededZeros > 0
    then fromCharArray (A.replicate neededZeros '0') <> s
    else s
