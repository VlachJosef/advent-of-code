module Year2017.Day9 where

-- http://adventofcode.com/2017/day/9

import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Lazy (fix)
import Data.Either (either)
import Data.Foldable (foldl)
import Data.Functor (map)
import Data.Generic.Rep as Generic
import Data.Generic.Rep.Show as Generic.Show
import Data.List (List(..))
import Data.Show (class Show)
import Data.String (length)
import Prelude (bind, ($), (*>), (+), (-), (<>))
import Text.Parsing.StringParser (Parser, runParser, try)
import Text.Parsing.StringParser.Combinators (between, many, sepBy)
import Text.Parsing.StringParser.String (anyChar, regex, string)

data Stream = Block (List Stream) | Garbage String

countGroup :: Stream -> Int
countGroup b = go 0 b where
  go acc (Block (Cons x xs)) = 1 + foldl (+) acc (map (go acc) xs) + (go acc x)
  go acc (Block Nil)         = 1 + acc
  go acc (Garbage _)         = acc

countScore :: Stream -> Int
countScore b = go 0 b where
  go acc (Block (Cons x xs)) = foldl (+) acc (map (go $ acc + 1) xs) + (go (acc + 1) x)
  go acc (Block Nil)         = acc
  go acc (Garbage _)         = 0

countGarbage :: Stream -> Int
countGarbage b = go 0 b where
  go acc (Block (Cons x xs)) = foldl (+) acc (map (go acc) xs) + (go acc x)
  go acc (Block Nil)         = 0
  go acc (Garbage s)         = acc + length s

pStream :: Parser Stream
pStream = fix $ \_ -> map Block (pBlockOrGarbage `sepBy` pDelim)

pDelim :: Parser String
pDelim = string ","

pBlockOrGarbage :: Parser Stream
pBlockOrGarbage = fix $ \_ -> pBlock <|> pGarbage

pBlock :: Parser Stream
pBlock = fix $ \_ -> pBetween "{" "}" pStream

pBetween :: forall a. String -> String -> Parser a -> Parser a
pBetween start end = between (string start) (string end)

pGarbage :: Parser Stream
pGarbage = map Garbage $ pBetween "<" ">" $ pGarbageEscaped <|> pGarbageNoEscaped

pGarbageEscaped :: Parser String
pGarbageEscaped = map (foldl (<>) "") $ try $ many pGarbageSegment

pGarbageSegment :: Parser String
pGarbageSegment = let escaped = regex "[^!>]*" in do
  x <- escaped
  _ <- string "!" *> anyChar
  y <- escaped
  pure $ x <> y

pGarbageNoEscaped :: Parser String
pGarbageNoEscaped = regex "[^>]*"

countGroups :: String -> Int
countGroups str = (calculate countGroup str) - 1

countScores :: String -> Int
countScores = calculate countScore

countGarbages :: String -> Int
countGarbages = calculate countGarbage

calculate :: (Stream -> Int) -> String -> Int
calculate  f str = either (\_ -> 0) f (runParser pStream str)

derive instance genericB :: Generic.Generic Stream _

instance showB :: Show Stream where
  show x = Generic.Show.genericShow x
