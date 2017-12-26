module Year2017.Day12.Types where

-- http://adventofcode.com/2017/day/12

import Data.Generic.Rep as Generic
import Data.Generic.Rep.Show as Generic.Show
import Data.List (List)
import Data.Map as M
import Data.Show (class Show)

data Pipe = Pipe {from :: Int, to :: List Int}

newtype Pipes = Pipes (M.Map Int (List Int))

derive instance genericPipe :: Generic.Generic Pipe _

instance showPipe :: Show Pipe where
   show = Generic.Show.genericShow
