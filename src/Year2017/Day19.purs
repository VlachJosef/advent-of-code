module Year2017.Day19 where

-- http://adventofcode.com/2017/day/19

import Control.Applicative (pure)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.State (State, execState, gets, modify, state)
import Data.Array (drop, index, length, snoc, unsafeIndex)
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.Newtype (over, unwrap)
import Data.String (Pattern(Pattern), fromCharArray, split, toCharArray)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unit (Unit, unit)
import Partial.Unsafe (unsafePartial)
import Prelude (bind, ($), (*>), (+), (-), (<$>), (<<<), (>>>))
import Year2017.Day19.Types (Diagram, Direction(..), Information(Information), Maze(..))

solution1 :: String -> Maybe String
solution1 str = fromCharArray <<< _.letters <<< unwrap <$> run str

solution2 :: String -> Maybe Int
solution2 str = _.steps <<< unwrap <$> run str

toDiagram :: String -> Diagram
toDiagram str = toCharArray >>> (toMaze <$> _) <$> drop 1 (split (Pattern "\n") str)

startingPosition :: Diagram -> Maybe Information
startingPosition diagram = do
  firstRow <- index diagram 0
  pure $ Information { direction: Down
                     , diagram: diagram
                     , letters: []
                     , x: 0
                     , y: length firstRow - 1
                     , steps: 0
                     }

mainLoop :: State Information Unit
mainLoop = tailRecM go unit where
  go :: Unit -> State Information (Step Unit Unit)
  go u = do
   action <- nextAction
   case action of
    (Loop Pipe)  -> moveOn
    (Loop Minus) -> moveOn
    (Loop Plus)  -> onPlus *> moveOn
    (Loop (Letter l)) -> onLetter l *> moveOn
    _ -> terminate

terminate :: State Information (Step Unit Unit)
terminate = state (\s -> (Done unit) /\ s)

nextAction :: State Information (Step Maze Unit)
nextAction = gets \s @ (Information{diagram, x, y}) ->
  Loop $ unsafePartial readCoordinateUnsafe (x /\ y) diagram

onLetter :: Char -> State Information (Step Unit Unit)
onLetter l = map Loop $ modify $ over Information \s @ {letters} ->
  s {letters = snoc letters l}

moveOn :: State Information (Step Unit Unit)
moveOn = map Loop $ modify $ over Information \s @ {direction, x, y, steps} ->
  case direction of
    Up    -> s {x = x - 1, steps = steps + 1}
    Down  -> s {x = x + 1, steps = steps + 1}
    Left  -> s {y = y - 1, steps = steps + 1}
    Right -> s {y = y + 1, steps = steps + 1}

onPlus :: State Information (Step Unit Unit)
onPlus = map Loop $ modify $ over Information \(s @ {direction, diagram, x, y}) -> let
  horizontalChange = let
    canGoRight = readCoordinate (x /\ (y + 1)) diagram
    in s { direction = case canGoRight of
              Just Empty -> Left
              Nothing    -> Left
              _          -> Right
         }
  verticalChange = let
    canGoDown = readCoordinate (x + 1 /\ y) diagram
    in s { direction = case canGoDown of
              Just Empty -> Up
              Nothing    -> Up
              _          -> Down
         }
  in case direction of
  Up    -> horizontalChange
  Down  -> horizontalChange
  Left  -> verticalChange
  Right -> verticalChange

readCoordinateUnsafe :: Partial => Int /\ Int -> Diagram -> Maze
readCoordinateUnsafe (x /\ y) diagram = let
  row = unsafeIndex diagram x
  in unsafeIndex row y

readCoordinate :: Int /\ Int -> Diagram -> Maybe Maze
readCoordinate (x /\ y) diagram = do
  row <- index diagram x
  index row y

run :: String -> Maybe Information
run str = execState mainLoop <$> (startingPosition <<< toDiagram) str

toMaze :: Char -> Maze
toMaze ' ' = Empty
toMaze '|' = Pipe
toMaze '+' = Plus
toMaze '-' = Minus
toMaze  l  = Letter l
