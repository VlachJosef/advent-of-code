module Year2017.Day20 where

-- http://adventofcode.com/2017/day/20

import Control.Applicative (pure)
import Data.Array (drop, (..))
import Data.Either (Either)
import Data.Foldable (foldl)
import Data.Functor (map)
import Data.Int (toNumber)
import Data.List (List(Nil), filter, groupBy, head, length, mapWithIndex, sort, (:))
import Data.List.NonEmpty (toList)
import Data.List.NonEmpty as NE
import Data.Maybe (Maybe)
import Data.Show (show)
import Data.String (trim)
import Data.Tuple.Nested (type (/\), (/\))
import Prelude (bind, const, ($), (*>), (+), (-), (<$>), (<*), (<<<), (<=), (<>), (==), (>>=))
import Text.Parsing.StringParser (ParseError, Parser, fail, runParser)
import Text.Parsing.StringParser.Combinators (between, sepBy, sepBy1)
import Text.Parsing.StringParser.String (char, skipSpaces)
import Year2017.Day20.Types (Particle(Particle), Vector(Vector))
import Year2017.Day8 (pInt)

type ParticleSegments = Vector /\ Vector /\ Vector

pVector :: Parser Vector
pVector = do
  xs <- between (char '<') (char '>') (sepBy1 pInt (char ','))
  case xs of
    (x : y : z : Nil) -> pure $ Vector {x:toNumber x, y:toNumber y, z:toNumber z}
    somethingElse -> fail ("Expected x,y,z coordinates, got: " <> show somethingElse)

pSegment :: Char -> Parser Vector
pSegment ch = char ch *> char '=' *> pVector

pParticle :: Parser ParticleSegments
pParticle = do
  position     <- pSegment 'p' <* char ',' <* skipSpaces
  velocity     <- pSegment 'v' <* char ',' <* skipSpaces
  acceleration <- pSegment 'a'
  pure $ position /\ velocity /\ acceleration

pParticles :: Parser (List Particle)
pParticles = mapWithIndex vectorsToParticle <$> pParticle `sepBy` char '\n'

vectorsToParticle :: Int -> ParticleSegments -> Particle
vectorsToParticle i (position /\ velocity /\ acceleration) =
  Particle { number : i
           , position: position
           , velocity: velocity
           , acceleration: acceleration
           }

moveVelocity :: Particle -> Particle
moveVelocity (Particle p@{velocity, acceleration}) =
  Particle p { velocity = add velocity acceleration
             }

movePosition :: Particle -> Particle
movePosition (Particle p@{position, velocity}) =
  Particle p { position = add position velocity
             }

add :: Vector -> Vector -> Vector
add (Vector v) (Vector{x,y,z}) =
  Vector v { x = v.x + x
           , y = v.y + y
           , z = v.z + z
           }

move :: Particle -> Particle
move = movePosition <<< moveVelocity

solution1 :: String -> Int -> Either ParseError (Maybe Int)
solution1 str iter = (head <<< orderedByDistance iter) <$> (runParser pParticles (trim str))

solution2 :: String -> Int -> Either ParseError Int
solution2 str iter = resolveCollision iter <$> (runParser pParticles (trim str))

orderedByDistance :: Int -> List Particle -> List Int
orderedByDistance iter xs = toPositions $ sort $ map (\j -> foldl (const <<< move) j (drop 1 (0 .. iter))) xs

resolveCollision :: Int -> List Particle -> Int
resolveCollision iter xs = go iter xs where
  go :: Int -> List Particle -> Int
  go 0 xss = length xss
  go n xss = go (n-1) (removeColliding $ map move xss)

removeColliding :: List Particle -> List Particle
removeColliding xs = let
  groupedByDistance = groupBy haveSamePosition xs
  uniqueByDistance  = filter (\nel -> (NE.length nel) <= 1) groupedByDistance
  in uniqueByDistance >>= toList

haveSamePosition :: Particle -> Particle -> Boolean
haveSamePosition (Particle {position: p1}) (Particle {position: p2}) = p1 == p2

toPositions :: List Particle -> List Int
toPositions xs = map number xs

number :: Particle -> Int
number (Particle p) = p.number
