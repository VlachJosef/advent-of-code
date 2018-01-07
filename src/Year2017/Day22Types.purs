module Year2017.Day22.Types where

-- http://adventofcode.com/2017/day/22

import Data.Eq (class Eq)
import Data.Generic.Rep as Generic
import Data.Newtype (class Newtype)
import Data.Ord (class Ord)
import Data.Show (class Show, show)
import Prelude ((<>))
import Year2017.Day19.Types (Direction)

data Node = C -- Clean
          | I -- Infected

data Node2 = Cl -- Clean
           | We -- Weakened
           | In -- Infected
           | Fl -- Flagged

newtype Grid a = Grid (Array (Array a))

newtype Information a = Information { direction :: Direction
                                    , grid :: Grid a
                                    , x :: Int
                                    , y :: Int
                                    , burstCounter :: Int
                                    }

derive instance newtypeGrid        :: Newtype (Grid a) _
derive instance newtypeInformation :: Newtype (Information a) _

derive instance genericNode :: Generic.Generic Node _

instance showNode :: Show Node where
  show C = "."
  show I = "#"

instance showNode2 :: Show Node2 where
  show Cl = "."
  show In = "#"
  show We = "W"
  show Fl = "F"

derive instance eqNode :: Eq Node
derive instance ordNode :: Ord Node

derive instance eqNode2 :: Eq Node2
derive instance ordNode2 :: Ord Node2

instance showInformation :: Show (Information a) where
  show (Information {direction, x, y, burstCounter }) =
       "Direction : " <> show direction
    <> "\nPosition: " <> show x <> "," <> show y
    <> "\nBursts  : " <> show burstCounter
