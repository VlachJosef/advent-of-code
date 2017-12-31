module Year2017.Day18.Types where

-- http://adventofcode.com/2017/day/18

import Data.Eq (class Eq)
import Data.Generic.Rep as Generic
import Data.Generic.Rep.Show as Generic.Show
import Data.Map (Map, insert)
import Data.Newtype (class Newtype, wrap)
import Data.Ord (class Ord)
import Data.Show (class Show)
import Prelude ((+))

type Memory = Map Reg Number

newtype Reg = Reg Char

data Source = Register Reg | Literal Int

data Instruction = SND Reg
                 | SET Reg Source
                 | ADD Reg Source
                 | MUL Reg Source
                 | MOD Reg Source
                 | RCV Reg
                 | JGZ Source Source

newtype State = State { memory :: Memory
                      , instructions :: Array Instruction
                      , lastSoundFrequency :: Number
                      , cursor :: Int
                      }

data ProgramId = ID0 | ID1

newtype State2 = State2 { memory :: Memory
                        , instructions :: Array Instruction
                        , receiveQueue :: Array Number
                        , cursor :: Int
                        , programId :: ProgramId
                        , receiveHistory :: Array Number
                        , programState :: ProgramState2
                        }

class HasMemory a where
  memory :: a -> Memory

instance stateMemory :: HasMemory State where
  memory (State state) = state.memory

instance state2Memory :: HasMemory State2 where
  memory (State2 state) = state.memory

class HasMemory a <= HasUpdatableMemory a  where
  updateMemory :: Reg -> Number -> a -> a

instance stateUpdatableMemory :: HasUpdatableMemory State where
  updateMemory reg number s@(State state) = wrap state { memory = insert reg number (memory s)
                                                       }

instance stateUpdatable2Memory :: HasUpdatableMemory State2 where
  updateMemory reg number s@(State2 state) = wrap state { memory = insert reg number (memory s)
                                                        }
class Movable a where
  move :: Int -> a -> a

instance movableState :: Movable State where
  move moveBy (State state) = wrap state { cursor = state.cursor + moveBy
                                         }

instance movableState2 :: Movable State2 where
  move moveBy (State2 state) = wrap state { cursor = state.cursor + moveBy
                                          }

data ProgramState = Continue State | Terminate State
data ProgramState2 = Running | Waiting

derive instance newtypeState     :: Newtype State _
derive instance newtypeState2    :: Newtype State2 _
derive instance newtypeReg       :: Newtype Reg _

derive instance genericReg         :: Generic.Generic Reg _
derive instance genericSource      :: Generic.Generic Source _
derive instance genericInstruction :: Generic.Generic Instruction _
derive instance genericState       :: Generic.Generic State _
derive instance genericState2      :: Generic.Generic State2 _
derive instance genericProgramId   :: Generic.Generic ProgramId _
derive instance genericProgramState2 :: Generic.Generic ProgramState2 _

instance showInstruction :: Show Instruction where
  show = Generic.Show.genericShow

instance showState :: Show State where
  show = Generic.Show.genericShow

instance showState2 :: Show State2 where
  show = Generic.Show.genericShow

instance showProgramState2 :: Show ProgramState2 where
  show = Generic.Show.genericShow

instance showProgramId :: Show ProgramId where
  show = Generic.Show.genericShow

instance showReg :: Show Reg where
  show = Generic.Show.genericShow

instance showSource :: Show Source where
  show = Generic.Show.genericShow

derive instance eqReg :: Eq Reg
derive instance ordReg :: Ord Reg
