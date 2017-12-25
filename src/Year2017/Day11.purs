module Year2017.Day11 where

-- http://adventofcode.com/2017/day/11

import Data.Array (init)
import Data.Foldable (foldl, maximum)
import Data.Functor (map)
import Data.Int (odd)
import Data.Maybe (Maybe(..))
import Data.Unit (Unit)
import Prelude (otherwise, (&&), (+), (-), (<), (<<<), (<>), (==), (>), ($))
import Year2017.Day11.Input (input)
import Year2017.Day11.Types (Move(..), Node(..))

inits :: forall a.Array a -> Array (Array a)
inits xs = go (Just xs) [] where
  go :: Maybe (Array a) -> Array (Array a) -> Array (Array a)
  go Nothing acc = acc
  go (Just xs) acc = go (init xs) ([xs] <> acc)

furthest :: Array Move -> Maybe Int
furthest xs = maximum $ map walkDistance (inits xs)

walkDistance :: Array Move -> Int
walkDistance = distance <<< walk

solution1 :: Unit -> Int
solution1 _ = walkDistance input

solution2 :: Unit -> Maybe Int
solution2 _ = furthest input

walk :: Array Move -> Node
walk = foldl nextMove (Node 0 0)

distance :: Node -> Int
distance n = go 0 n where
  go acc (Node 0 0)    = acc
  go acc (Node 1 0)    = 1 + acc
  go acc (Node (-1) 0) = 1 + acc
  go acc n@(Node x y)
    | x < 0 && y == 0 = go (acc + 1) (nextMove n S)
    | x < 0 && y <  0 = go (acc + 1) (nextMove n SE)
    | x < 0 && y >  0 = go (acc + 1) (nextMove n SW)
    | x > 0 && y == 0 = go (acc + 1) (nextMove n N)
    | x > 0 && y <  0 = go (acc + 1) (nextMove n NE)
    | x > 0 && y >  0 = go (acc + 1) (nextMove n NW)
    | otherwise = acc

nextMove :: Node -> Move -> Node
nextMove (Node x y) NW
  | odd x == true     = Node (x - 1) y
  | otherwise         = Node (x - 1) (y - 1)
nextMove (Node x y) N = Node (x - 2) y
nextMove (Node x y) NE
  | odd x == true     = Node (x - 1) (y + 1)
  | otherwise         = Node (x - 1) y
nextMove (Node x y) SW
  | odd x == true     = Node (x + 1) y
  | otherwise         = Node (x + 1) (y - 1)
nextMove (Node x y) S = Node (x + 2) y
nextMove (Node x y) SE
  | odd x == true     = Node (x + 1) (y + 1)
  | otherwise         = Node (x + 1) y
