module Year2017.Day20.Types where

-- http://adventofcode.com/2017/day/20

import Data.Eq (class Eq)
import Data.Foldable (sum)
import Data.Generic.Rep as Generic
import Data.Generic.Rep.Eq as Generic.Eq
import Data.Newtype (class Newtype)
import Data.Ord (class Ord, abs, compare)
import Data.Show (class Show, show)
import Prelude ((<>), ($), (<$>))

newtype Vector = Vector{ x :: Number
                       , y :: Number
                       , z :: Number
                       }

data Particle = Particle { number :: Int
                         , position :: Vector
                         , velocity :: Vector
                         , acceleration :: Vector
                         }

derive instance newtypeInformation :: Newtype Vector _

derive instance genericParticle :: Generic.Generic Particle _
derive instance genericVector   :: Generic.Generic Vector _

instance eqParticle :: Eq Particle where
  eq = Generic.Eq.genericEq

instance eqVector :: Eq Vector where
  eq = Generic.Eq.genericEq

instance ordParticle :: Ord Particle where
  compare p1 p2 = compare (distance p1) (distance p2)

instance showParticle :: Show Particle where
  show (p@Particle {number, position, velocity, acceleration}) = "P #" <> show number
                                                               <> ", p=" <> show position
                                                               <> ", v=" <> show velocity
                                                               <> ", a=" <> show acceleration
                                                               <> ", dist=" <> (show $ distance p)

distance :: Particle -> Number
distance (Particle {position: Vector{x, y, z}}) = sum $ abs <$> [x, y, z]

instance showVector :: Show Vector where
  show (Vector{x, y, z}) = show x <> "," <>
                           show y <> "," <>
                           show y
