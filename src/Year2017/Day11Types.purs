module Year2017.Day11.Types where

-- http://adventofcode.com/2017/day/11

import Data.Eq (class Eq)
import Data.Generic.Rep as Generic
import Data.Generic.Rep.Eq as Generic.Eq
import Data.Generic.Rep.Show as Generic.Show
import Data.Maybe (Maybe(..))
import Data.Show (class Show)

data Node = Node Int Int

data Move = NW | N | NE
          | SW | S | SE

toGrid :: String -> Maybe Move
toGrid "nw"      = Just NW
toGrid "n"       = Just N
toGrid "ne"      = Just NE
toGrid "sw"      = Just SW
toGrid "s"       = Just S
toGrid "se"      = Just SE
toGrid otherwise = Nothing

derive instance genericGrid :: Generic.Generic Move _
derive instance genericNode :: Generic.Generic Node _

instance showGrid :: Show Move where
   show = Generic.Show.genericShow

instance eqGrid :: Eq Move where
  eq = Generic.Eq.genericEq

instance eqNode :: Eq Node where
  eq = Generic.Eq.genericEq

instance showNode :: Show Node where
   show = Generic.Show.genericShow
