module Year2017.Day10.Types where

-- http://adventofcode.com/2017/day/10

import Data.Generic.Rep as Generic
import Data.Generic.Rep.Show as Generic.Show
import Data.Monoid (class Monoid, class Semigroup)
import Data.Newtype (class Newtype)
import Data.Show (class Show)
import Prelude ((+))

newtype CurrentPosition = CurrentPosition Int
newtype Skip            = Skip Int
newtype Length          = Length Int
newtype Input           = Input (Array Int)
newtype Lengths         = Lengths (Array Length)
newtype DenseHash       = DenseHash String

derive instance newtypeCurrentPosition :: Newtype CurrentPosition _
derive instance newtypeSkip            :: Newtype Skip _
derive instance newtypeInput           :: Newtype Input _
derive instance newtypeLenght          :: Newtype Length _
derive instance newtypeLenghts         :: Newtype Lengths _
derive instance newtypeDenseHash       :: Newtype DenseHash _
derive instance genericLength          :: Generic.Generic Length _

instance showLength :: Show Length where
   show = Generic.Show.genericShow

derive instance genericInput :: Generic.Generic Input _

instance showInput :: Show Input where
   show = Generic.Show.genericShow

derive instance genericLengths :: Generic.Generic Lengths _

instance showLengths :: Show Lengths where
   show = Generic.Show.genericShow

instance semigroupSkip :: Semigroup Skip where
  append (Skip a) (Skip b) = Skip (a + b)

instance monoidSkip :: Monoid Skip where
  mempty = Skip 0

instance semigroupCurrentPosition :: Semigroup CurrentPosition where
  append (CurrentPosition a) (CurrentPosition b) = CurrentPosition (a + b)

instance monoidCurrentPosition :: Monoid CurrentPosition where
  mempty = CurrentPosition 0
