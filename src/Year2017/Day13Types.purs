module Year2017.Day13.Types where

-- http://adventofcode.com/2017/day/13

import Data.Eq (class Eq)
import Data.Generic.Rep as Generic
import Data.Generic.Rep.Eq as Generic.Eq
import Data.Generic.Rep.Show as Generic.Show
import Data.List (List)
import Data.Ord (class Ord, compare)
import Data.Show (class Show)
import Data.Tuple.Nested (type (/\))

data Layer = Layer {depth :: Int, range :: Int}

data Direction = Asc
               | Desc

data LayerState = LayerEmpty Int
                | LayerState { direction :: Direction
                             , position :: Int
                             , layer :: Layer
                             }

data WorldState = WorldState { playerPosition :: Int
                             , collision :: Array (Int /\ Int)
                             , layerStates :: Array LayerState
                             }

instance ordLayerState :: Ord LayerState where
  compare (LayerState {direction: d1, position: p1, layer: (Layer l1)})
          (LayerState {direction: d2, position: p2, layer: (Layer l2)}) = compare l1.depth l2.depth
  compare (LayerEmpty x1)
          (LayerState {direction: d2, position: p2, layer: (Layer l2)}) = compare x1 l2.depth
  compare (LayerState {direction: d1, position: p1, layer: (Layer l1)})
          (LayerEmpty x2) = compare l1.depth x2
  compare (LayerEmpty x1)
          (LayerEmpty x2) = compare x1 x2

derive instance genericLayerState :: Generic.Generic LayerState _
derive instance genericDirection :: Generic.Generic Direction _
derive instance genericLayer :: Generic.Generic Layer _
derive instance genericWorldState :: Generic.Generic WorldState _

instance showLayer :: Show Layer where
   show = Generic.Show.genericShow

instance showWorldState :: Show WorldState where
   show = Generic.Show.genericShow

instance showDirection :: Show Direction where
   show = Generic.Show.genericShow

instance showLayerState :: Show LayerState where
   show = Generic.Show.genericShow

instance eqLayerState :: Eq LayerState where
  eq = Generic.Eq.genericEq

instance eqDirection :: Eq Direction where
  eq = Generic.Eq.genericEq

instance eqLayer :: Eq Layer where
  eq = Generic.Eq.genericEq
