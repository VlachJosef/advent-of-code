module Year2017.Day14.Types where

-- http://adventofcode.com/2017/day/14

import Data.Eq (class Eq)
import Data.Generic.Rep as Generic
import Data.Generic.Rep.Eq as Generic.Eq
import Data.Generic.Rep.Show as Generic.Show
import Data.Show (class Show)

data XO = X | O | N Int

derive instance genericLayer :: Generic.Generic XO _

instance showXO :: Show XO where
   show = Generic.Show.genericShow

instance eqXO :: Eq XO where
  eq = Generic.Eq.genericEq
