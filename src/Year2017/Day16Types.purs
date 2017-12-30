module Year2017.Day16.Types where

-- http://adventofcode.com/2017/day/16

import Data.Eq (class Eq)
import Data.Generic.Rep as Generic
import Data.Generic.Rep.Eq as Generic.Eq
import Data.Generic.Rep.Show as Generic.Show
import Data.Show (class Show)

data Dance = Spin Int | Exchange Int Int | Partner Char Char
data LoopDetector = Continue | Terminate

derive instance genericDance :: Generic.Generic Dance _
derive instance genericLoopDetector :: Generic.Generic LoopDetector _

instance showDance :: Show Dance where
   show = Generic.Show.genericShow

instance eqDance :: Eq Dance where
  eq = Generic.Eq.genericEq

instance eqLoopDetector :: Eq LoopDetector where
  eq = Generic.Eq.genericEq
