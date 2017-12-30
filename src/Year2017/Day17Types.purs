module Year2017.Day17.Types where

-- http://adventofcode.com/2017/day/17

import Data.Eq (class Eq)
import Data.Generic.Rep as Generic
import Data.Generic.Rep.Eq as Generic.Eq
import Data.Generic.Rep.Show as Generic.Show
import Data.Newtype (class Newtype)
import Data.Show (class Show)

newtype State a = State { currentPosition :: CurrentPosition
                        , array :: Array a
                        }
newtype StateLike = StateLike { currentPosition :: CurrentPosition
                              , nextToZero :: Int
                              }

newtype NextElement     = NextElement Int
newtype CurrentPosition = CurrentPosition Int
newtype NumberOfSteps   = NumberOfSteps Int

derive instance newtypeNextElement     :: Newtype NextElement _
derive instance newtypeCurrentPosition :: Newtype CurrentPosition _
derive instance newtypeNumberOfSteps   :: Newtype NumberOfSteps _
derive instance newtypeState           :: Newtype (State a) _
derive instance newtypeStateLike       :: Newtype StateLike _

derive instance genericCurrentPosition :: Generic.Generic CurrentPosition _
derive instance genericState           :: Generic.Generic (State a) _
derive instance genericStateLike       :: Generic.Generic StateLike _

instance showCurrentPosition :: Show CurrentPosition where
  show = Generic.Show.genericShow

instance showState :: Show a => Show (State a) where
  show = Generic.Show.genericShow

instance showStateLike :: Show StateLike where
  show = Generic.Show.genericShow

instance eqCurrentPosition :: Eq CurrentPosition where
  eq = Generic.Eq.genericEq

derive instance eqState :: Eq a => Eq (State a)

derive instance eqStateLike :: Eq StateLike
