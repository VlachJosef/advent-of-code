module Year2017.Day12 where

-- http://adventofcode.com/2017/day/12

import Control.Applicative (pure)
import Data.Array (concatMap, cons, elemIndex, fromFoldable, length, nub, sort)
import Data.Either (either)
import Data.Function (flip, id)
import Data.Functor (map)
import Data.Int (fromString)
import Data.List (List(Nil), filter)
import Data.Map as M
import Data.Maybe (Maybe(Nothing, Just), isNothing, maybe)
import Data.String (fromCharArray)
import Data.Tuple.Nested (type (/\), (/\))
import Prelude (bind, ($), (*>), (<<<), (<>))
import Text.Parsing.StringParser (Parser, fail, runParser)
import Text.Parsing.StringParser.Combinators (many1, sepBy)
import Text.Parsing.StringParser.String (anyDigit, skipSpaces, string)
import Year2017.Day12.Types (Pipe(..), Pipes(..))

all :: Pipes -> Array (Array Int)
all p@(Pipes pipes) = let
  keys   = fromFoldable (M.keys pipes)
  groups = map (sort <<< nub <<< flip groupFor p) keys
  in nub groups

groupFor :: Int -> Pipes -> Array Int
groupFor start (Pipes pipes) = go start [] where
  go :: Int -> Array Int -> Array Int
  go current seen = case M.lookup current pipes of
    Just xs -> let
      getNotYetSeen = fromFoldable <<< filter (isNothing <<< (flip elemIndex) seen)
      seenUpd       = cons current seen
      in case getNotYetSeen xs of
        []         -> seenUpd
        notYetSeen -> concatMap (flip go seenUpd) notYetSeen
    Nothing -> [] -- this case indicates error in input data

solution1 :: String -> Int
solution1 = length <<< sort <<< nub <<< groupFor 0 <<< toLookup <<< parseInput

solution2 :: String -> Int
solution2 = length <<< all <<< toLookup <<< parseInput

toLookup :: List Pipe -> Pipes
toLookup xs = Pipes $ M.fromFoldable (map pipeToTuple xs)

pipeToTuple :: Pipe -> Int /\ (List Int)
pipeToTuple (Pipe {from, to}) = from /\ to

parseInput :: String -> List Pipe
parseInput str = either (\_ -> Nil) id $ runParser pInput str

pDigits :: Parser String
pDigits = map fromCharArray (map fromFoldable $ many1 anyDigit)

pIntPositive :: Parser Int
pIntPositive = do
  digits <- pDigits
  maybe (fail $ "Expected a number, got " <> digits) pure (fromString digits)

pInput :: Parser (List Pipe)
pInput = pPipe `sepBy` string "\n"

pPipe :: Parser Pipe
pPipe = do
  from <- skipSpaces *> pIntPositive
  _    <- skipSpaces *> string "<->"
  to   <- skipSpaces *> pIntPositive `sepBy` (string ", ")
  pure $ Pipe {from, to}
