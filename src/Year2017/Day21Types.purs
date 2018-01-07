module Year2017.Day21.Types where

-- http://adventofcode.com/2017/day/21

import Data.Eq (class Eq)
import Data.Generic.Rep as Generic
import Data.Generic.Rep.Show as Generic.Show
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Ord (class Ord)
import Data.Show (class Show)

data Pixel = On | Off

newtype Pattern = Pattern (Array (Array Pixel))

data RewriteRule = RewriteRule Pattern Pattern

type RuleLookup = Map Pattern Pattern

derive instance newtypePattern :: Newtype Pattern _

derive instance genericPattern :: Generic.Generic Pattern _
derive instance genericPixel :: Generic.Generic Pixel _
derive instance genericRewriteRule :: Generic.Generic RewriteRule _

instance showPixel :: Show Pixel where
  show = Generic.Show.genericShow

instance showPattern :: Show Pattern where
  show = Generic.Show.genericShow

instance showRewriteRule :: Show RewriteRule where
  show = Generic.Show.genericShow

derive instance eqPixel :: Eq Pixel
derive instance ordPixel :: Ord Pixel
derive instance eqPattern :: Eq Pattern
derive instance ordPattern :: Ord Pattern
derive instance eqRewriteRule :: Eq RewriteRule
