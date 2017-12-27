module Year2017.Day13 where

-- http://adventofcode.com/2017/day/13

import Control.Applicative (pure)
import Data.Array (concatMap, fromFoldable, length, null)
import Data.Boolean (otherwise)
import Data.Either (either)
import Data.Foldable (foldl, maximum, minimum, sum)
import Data.Function (const, id)
import Data.Functor (map)
import Data.List (List(Nil), difference, sort, (..))
import Data.Maybe (maybe)
import Data.Tuple.Nested (type (/\), (/\))
import Prelude (bind, ($), (&&), (*), (*>), (+), (-), (<$>), (<*>), (<<<), (<>), (==))
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.Combinators (sepBy)
import Text.Parsing.StringParser.String (skipSpaces, string)
import Year2017.Day8 (pIntPositive)
import Year2017.Day13.Types (Direction(..), Layer(..), LayerState(..), WorldState(..))

initial :: List Layer -> WorldState
initial xs = let
  knownDepths = map (\(Layer{depth}) -> depth) xs
  start       = minimum knownDepths
  end         = maximum knownDepths
  fullRange   = (..) <$> start <*> end
  diff        = difference (maybe Nil id fullRange) knownDepths
  emptyLayers = map LayerEmpty diff
  allLayers   = map initialLayer xs <> emptyLayers
  sorted      = sort allLayers
  in WorldState { playerPosition: 0
                , collision: []
                , layerStates: fromFoldable sorted
                }

initialLayer :: Layer -> LayerState
initialLayer layer = LayerState { direction: Desc
                                , position: 0
                                , layer: layer
                                }

solution1 :: String -> Int
solution1 input = let
  initialState @ (WorldState ws) = initial $ parseInput input
  totalLayers = length ws.layerStates
  (WorldState ws) = foldl (const <<< step) initialState (0 .. totalLayers)
  in sum $ map (\(a /\ b) -> a * b) ws.collision

solution2 :: String -> Int
solution2 input = let

  initialState @ (WorldState ws) = initial $ parseInput input
  totalLayers = length ws.layerStates

  go :: Int -> WorldState -> Int
  go iteration ws = let
    (WorldState wsRun) = foldl (const <<< stepIfNoCollision) ws (1 .. totalLayers)
    in if null wsRun.collision
       then iteration
       else go (iteration + 1) (stepOnly ws)

  in go 0 initialState

stepIfNoCollision :: WorldState -> WorldState
stepIfNoCollision ws @ (WorldState {collision}) =
  if null collision then step ws else ws

stepOnly :: WorldState -> WorldState
stepOnly (WorldState {layerStates}) = let
  ls = map tick layerStates
  in WorldState {playerPosition: 0, collision: [], layerStates: ls}

step :: WorldState -> WorldState
step (WorldState {playerPosition, collision, layerStates}) = let
  collisionNew =  concatMap (collissionData playerPosition) layerStates
  ls = map tick layerStates
  in WorldState {playerPosition: playerPosition + 1, collision: collision <> collisionNew, layerStates: ls} where

    collissionData :: Int -> LayerState -> Array (Int /\ Int)
    collissionData playerPosition (LayerState {position, layer:(Layer layer)}) =
      if 0 == position && layer.depth == playerPosition then
        [(playerPosition /\ layer.range)]
      else
        []
    collissionData _ _ = []

tick :: LayerState -> LayerState
tick (LayerState {direction, position, layer: l @ Layer {range}})
  | direction == Asc  && position == 0        = LayerState {direction: Desc, position: position + 1, layer: l}
  | direction == Desc && position == range -1 = LayerState {direction: Asc,  position: position - 1, layer: l}
  | direction == Asc                          = LayerState {direction: Asc,  position: position - 1, layer: l}
  | otherwise                                 = LayerState {direction: Desc, position: position + 1, layer: l}
tick other = other

parseInput :: String -> List Layer
parseInput str = either (\_ -> Nil) id $ runParser pLayers str

pLayers :: Parser (List Layer)
pLayers = pLayer `sepBy` string "\n"

pLayer :: Parser Layer
pLayer = do
  depth <- skipSpaces *> pIntPositive
  _     <- skipSpaces *> string ":"
  range <- skipSpaces *> pIntPositive
  pure $ Layer {depth, range}
