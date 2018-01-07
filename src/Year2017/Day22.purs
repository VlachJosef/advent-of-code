module Year2017.Day22 where

-- http://adventofcode.com/2017/day/22

import Control.Alt ((<|>))
import Control.Monad.State (State, execState, modify)
import Data.Array (alterAt, drop, fromFoldable, index, length, modifyAt, replicate, unsafeIndex, (..))
import Data.Either (Either)
import Data.Eq (class Eq)
import Data.Foldable (foldl)
import Data.Function (flip, id)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (over, unwrap)
import Data.Sequence.Internal ((<$$>))
import Data.String (trim)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unit (Unit)
import Partial.Unsafe (unsafePartial)
import Prelude (bind, const, ($), ($>), (+), (-), (/), (/=), (<$>), (<<<), (<>), (==), (>>=))
import Text.Parsing.StringParser (ParseError, Parser, runParser)
import Text.Parsing.StringParser.Combinators (many1, sepBy)
import Text.Parsing.StringParser.String (char)
import Year2017.Day19.Types (Direction(..))
import Year2017.Day22.Types (Grid(..), Information(..), Node(..), Node2(..))

pNode :: Parser Node
pNode = pClean <|> pInfected where
  pClean    = char '.' $> C
  pInfected = char '#' $> I

pGrid :: Parser (Grid Node)
pGrid = Grid <$> (fromFoldable <$$> fromFoldable <$> many1 pNode `sepBy` char '\n')

defaultDirection :: Direction
defaultDirection = Up

solution1 :: String -> Int -> Either ParseError Int
solution1 str i = infoOnly <<< run i mainLoop1 <$> runParser pGrid (trim str)

solution2 :: String -> Int -> Either ParseError Int
solution2 str i = infoOnly <<< run i mainLoop2 <<< gridToGrid2 <$> runParser pGrid (trim str)

infoOnly :: forall a. Information a -> Int
infoOnly (Information {burstCounter}) = burstCounter

initialState :: forall a. Grid a -> Information a
initialState grid = let
  middle = length (unwrap grid) / 2
  in Information { direction: Up
                 , grid: grid
                 , x: middle
                 , y: middle
                 , burstCounter: 0
                 }

gridToGrid2 :: Grid Node -> Grid Node2
gridToGrid2 (Grid g) = Grid $ nodeToNode2 <$$> g where

  nodeToNode2 :: Node -> Node2
  nodeToNode2 C = Cl
  nodeToNode2 I = In

run :: forall a. Int -> State (Information a) Unit -> Grid a -> Information a
run i loop grid = foldl (const <<< execState loop) (initialState grid) (drop 1 (0 .. i))

mainLoop1 :: State (Information Node) Unit
mainLoop1 = mainLoop C C turn swap where

  swap :: Node -> Node
  swap C = I
  swap I = C

  turn :: Node -> Direction -> Direction
  turn n direction = case direction /\ n of
    Up    /\ C -> Left
    Up    /\ I -> Right
    Down  /\ C -> Right
    Down  /\ I -> Left
    Left  /\ C -> Down
    Left  /\ I -> Up
    Right /\ C -> Up
    Right /\ I -> Down

mainLoop2 :: State (Information Node2) Unit
mainLoop2 = mainLoop Cl We turn swap where

  swap :: Node2 -> Node2
  swap Cl = We
  swap We = In
  swap In = Fl
  swap Fl = Cl

  turn :: Node2 -> Direction -> Direction
  turn n direction = case n of
    Cl -> case direction of
      Up    -> Left
      Down  -> Right
      Left  -> Down
      Right -> Up
    We -> direction
    In -> case direction of
       Up    -> Right
       Down  -> Left
       Left  -> Up
       Right -> Down
    Fl -> case direction of
      Up    -> Down
      Down  -> Up
      Left  -> Right
      Right -> Left

mainLoop :: forall a. Eq a => a -> a -> (a -> Direction -> Direction) -> (a -> a) -> State (Information a) Unit
mainLoop clean burstElement turn swap = do
  _ <- chooseDirection burstElement turn
  _ <- infectOrClean swap
  moveOn clean

chooseDirection :: forall a. Eq a => a -> (a -> Direction -> Direction) -> State (Information a) Unit
chooseDirection burstElement turn = modify $ over Information \s @ {x, y, grid, direction, burstCounter} -> let
  currentNode    = unsafePartial currentNodeUnsafe (x /\ y) grid
  burstIncrement = if (currentNode == burstElement) then 1 else 0
  in s { direction    = turn currentNode direction
       , burstCounter = burstCounter + burstIncrement
       }

infectOrClean :: forall a. (a -> a) -> State (Information a) Unit
infectOrClean swap = modify $ over Information \s @ {x, y, grid} -> let
  updatedGrid = over Grid (\row -> maybe [] id (alterAt x ((modifyAt y swap)) row)) grid
  in s {grid = updatedGrid}

moveOn :: forall a. a -> State (Information a) Unit
moveOn a = modify $ over Information \s @ {direction, x, y, grid} -> let
  position@(x1 /\ y1) = nextPosition (x /\ y) direction
  expandedGrid        = over Grid (growIfNeeded position a) grid
  correction          = if (length (unwrap expandedGrid) /= length (unwrap grid)) then 1 else 0
  in s {x = x1 + correction, y = y1 + correction, grid = expandedGrid}

currentNodeUnsafe :: forall a. Partial => Int /\ Int -> (Grid a) -> a
currentNodeUnsafe (x /\ y) (Grid xs) =
  unsafeIndex (unsafeIndex xs x) y

nextPosition :: Int /\ Int -> Direction -> Int /\ Int
nextPosition (x /\ y) = case _ of
  Up    -> x - 1 /\ y
  Down  -> x + 1 /\ y
  Left  -> x /\ (y - 1)
  Right -> x /\ (y + 1)

growIfNeeded :: forall a. Int /\ Int -> a -> Array (Array a) -> Array (Array a)
growIfNeeded (x /\ y) a xs = let
  row = index xs x >>= flip index y
  in case row of
    Nothing -> expandArray a xs
    _ -> xs

expandArray :: forall a. a -> Array (Array a) -> Array (Array a)
expandArray a xs = let
  topBottom = replicate (2 + length xs) a
  middle    = wrapBy a <$> xs
  in wrapBy topBottom middle

wrapBy :: forall a. a -> Array a -> Array a
wrapBy a xs = [a] <> xs <> [a]
