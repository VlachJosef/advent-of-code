module Year2017.Day19.Types where

-- http://adventofcode.com/2017/day/19

import Data.Generic.Rep as Generic
import Data.Generic.Rep.Show as Generic.Show
import Data.Newtype (class Newtype)
import Data.Show (class Show, show)
import Data.String (fromCharArray)
import Prelude ((<>))

data Direction = Up | Down | Left | Right
data Maze = Empty | Pipe | Plus | Minus | Letter Char

type Diagram = Array (Array Maze)
type Letters = Array Char

newtype Information = Information { direction :: Direction
                                  , diagram :: Diagram
                                  , letters :: Letters
                                  , x :: Int
                                  , y :: Int
                                  , steps :: Int
                                  }

instance showMaze :: Show Maze where
  show Empty      = " "
  show Pipe       = "|"
  show Plus       = "+"
  show Minus      = "-"
  show (Letter l) = show l

derive instance newtypeInformation :: Newtype Information _

derive instance genericInformation :: Generic.Generic Information _
derive instance genericDirection   :: Generic.Generic Direction _

instance showInformation :: Show Information where
  show (Information {direction, letters, x, y, steps}) =
       "Direction : " <> show direction
    <> "\nLetters : " <> fromCharArray letters
    <> "\nPosition: " <> show x <> "," <> show y
    <> "\nSteps   : " <> show steps

instance showDirection :: Show Direction where
  show = Generic.Show.genericShow
