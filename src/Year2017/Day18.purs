module Year2017.Day18 where

-- http://adventofcode.com/2017/day/18

import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array (cons, index, length, nub, snoc, sort, uncons)
import Data.Array as A
import Data.Either (Either)
import Data.Function (flip)
import Data.Functor (map)
import Data.Int (fromNumber, toNumber)
import Data.List (List(..), (:))
import Data.Map (fromFoldable, insert, lookup)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.String (trim)
import Data.Tuple (swap)
import Data.Tuple.Nested (type (/\), (/\))
import Math (remainder)
import Prelude (bind, ($), (*), (*>), (+), (<$>), (<<<), (<=), (==), (>>=))
import Text.Parsing.StringParser (ParseError, Parser, runParser)
import Text.Parsing.StringParser.Combinators (choice, sepBy)
import Text.Parsing.StringParser.String (anyChar, char, skipSpaces, string)
import Year2017.Day8 (pInt)
import Year2017.Day18.Types (class HasMemory, class HasUpdatableMemory, class Movable, Instruction(..), Memory, ProgramId(..), ProgramState(..), ProgramState2(..), Reg(..), Source(..), State(..), State2(..), memory, move, updateMemory)

solution1 :: String -> Either ParseError (Maybe Number)
solution1 str = do
  instructions <- runParser pInstructions (trim str)
  let regs  = registers instructions
      state = initialState (initialMemory regs) instructions
  pure $ map (\(State state) -> state.lastSoundFrequency) (interpret state)

solution2 :: String -> Either ParseError (Maybe Int)
solution2 str = do
  instructions <- runParser pInstructions (trim str)
  let regs     = registers instructions
      memory   = initialMemory regs
      stateId0 = initialState2 memory instructions ID0
      stateId1 = initialState2 memory instructions ID1
  pure $ do
    (State2 st0) /\ (State2 st1) <- interpret2 stateId0 stateId1
    pure $ length st0.receiveHistory

pInstructions :: Parser (List Instruction)
pInstructions = pInstruction `sepBy` char '\n'

pSourceLiteral :: Parser Source
pSourceLiteral = Literal <$> pInt

pSourceReg :: Parser Source
pSourceReg = Register <$> pReg

pReg :: Parser Reg
pReg = Reg <$> anyChar

pSource :: Parser Source
pSource = pSourceLiteral <|> pSourceReg

pInstruction :: Parser Instruction
pInstruction = choice [pSnd, pRcv, pSet, pAdd, pMul, pMod, pJgz]

pSnd :: Parser Instruction
pSnd = pInstructionOfRegOnly "snd" SND

pRcv :: Parser Instruction
pRcv = pInstructionOfRegOnly "rcv" RCV

pSet :: Parser Instruction
pSet = pInstructionOfReg "set" SET

pAdd :: Parser Instruction
pAdd = pInstructionOfReg "add" ADD

pMul :: Parser Instruction
pMul = pInstructionOfReg "mul" MUL

pMod :: Parser Instruction
pMod = pInstructionOfReg "mod" MOD

pJgz :: Parser Instruction
pJgz = pInstructionOfSource "jgz" JGZ

pInstructionOfReg :: String -> (Reg -> Source -> Instruction) -> Parser Instruction
pInstructionOfReg = pInstructionOfGen pReg

pInstructionOfSource :: String -> (Source -> Source -> Instruction) -> Parser Instruction
pInstructionOfSource = pInstructionOfGen pSource

pInstructionOfGen :: forall a. Parser a -> String -> (a -> Source -> Instruction) -> Parser Instruction
pInstructionOfGen pa str rsi = do
  reg    <- string str *> skipSpaces *> pa
  source <- skipSpaces *> pSource
  pure $ rsi reg source

pInstructionOfRegOnly :: String -> (Reg -> Instruction) -> Parser Instruction
pInstructionOfRegOnly str ri = string str *> skipSpaces *> anyChar >>= pure <<< ri <<< Reg

registers :: List Instruction -> Array Reg
registers inst = go inst [] where
  go :: List Instruction -> Array Reg -> Array Reg
  go Nil acc = sort $ nub acc
  go (h:t) acc = case h of
    SND reg              -> go t (cons reg acc)
    SET reg _            -> go t (cons reg acc)
    ADD reg _            -> go t (cons reg acc)
    MUL reg _            -> go t (cons reg acc)
    MOD reg _            -> go t (cons reg acc)
    RCV reg              -> go t (cons reg acc)
    JGZ (Register reg) _ -> go t (cons reg acc)
    JGZ (Literal _) _    -> go t acc

initialMemory :: Array Reg -> Memory
initialMemory regs = fromFoldable $ (_ /\ 0.0) <$> regs

initialState :: Memory -> List Instruction -> State
initialState memory instructions = State { memory
                                         , instructions: A.fromFoldable instructions
                                         , lastSoundFrequency: 0.0
                                         , cursor: 0
                                         }

initalValueOfRegisterP :: ProgramId -> Number
initalValueOfRegisterP ID0 = 0.0
initalValueOfRegisterP ID1 = 1.0

initialState2 :: Memory -> List Instruction -> ProgramId -> State2
initialState2 memory instructions programId = State2 { memory: insert (Reg 'p') (initalValueOfRegisterP programId) memory
                                                     , instructions: A.fromFoldable instructions
                                                     , receiveQueue: []
                                                     , cursor: 0
                                                     , programId: programId
                                                     , receiveHistory: []
                                                     , programState: Running
                                                     }

interpret :: State -> Maybe State
interpret state = do
  nextInstruction <- readNextInstruction state
  programState    <- interpretInstruction state nextInstruction
  case programState of
    Continue stateNext  -> interpret stateNext
    Terminate stateNext -> Just stateNext

stateOfPrograms :: State2 /\ State2 -> ProgramState2 /\ ProgramState2
stateOfPrograms (State2 s1 /\ State2 s0) = s0.programState /\ s1.programState

interpretNextInstruction :: State2 /\ State2 -> Maybe (State2 /\ State2)
interpretNextInstruction (s0 /\ s1) = do
  nextInstruction <- readNextInstruction2 s0
  interpretInstruction2 s0 s1 nextInstruction

interpret2 :: State2 -> State2 -> Maybe (State2 /\ State2)
interpret2 stateId0 stateId1 = tailRecM go (stateId0 /\ stateId1) where
  go :: State2 /\ State2 -> Maybe (Step (State2 /\ State2) (State2 /\ State2))
  go states = do
    statesUpdated <- interpretNextInstruction states >>= interpretNextInstruction <<< swap

    pure $ let
      nextStep = case stateOfPrograms statesUpdated of
        Waiting /\ Waiting -> Done
        _                  -> Loop
      in nextStep (swap statesUpdated)

readNextInstruction :: State -> Maybe Instruction
readNextInstruction (State {instructions, cursor}) = index instructions cursor

readNextInstruction2 :: State2 -> Maybe Instruction
readNextInstruction2 (State2 {instructions, cursor}) = index instructions cursor

fetchRegisterValue :: forall a. HasMemory a => a -> Reg -> Maybe Number
fetchRegisterValue a reg = lookup reg (memory a)

setRegisterValue :: forall a. HasUpdatableMemory a => a -> Reg -> Number -> a
setRegisterValue a reg frequency = updateMemory reg frequency a

lift :: State -> ProgramState
lift = Continue

interpretInstruction :: State -> Instruction -> Maybe ProgramState
interpretInstruction state = case _ of
  SND reg        -> lift <<< moveForward <$> execSound state reg
  SET reg source -> lift <<< moveForward <$> execSET state reg source
  ADD reg source -> lift <<< moveForward <$> execADD state reg source
  MUL reg source -> lift <<< moveForward <$> execMUL state reg source
  MOD reg source -> lift <<< moveForward <$> execMOD state reg source
  RCV reg        -> execRecover state reg
  JGZ cond jump  -> lift <$> execJGZ state cond jump

interpretInstruction2 :: State2 -> State2 -> Instruction -> Maybe (State2 /\ State2)
interpretInstruction2 from to = case _ of
  SND reg        -> execSend from to reg
  SET reg source -> (_ /\ to) <<< moveForward <$> execSET from reg source
  ADD reg source -> (_ /\ to) <<< moveForward <$> execADD from reg source
  MUL reg source -> (_ /\ to) <<< moveForward <$> execMUL from reg source
  MOD reg source -> (_ /\ to) <<< moveForward <$> execMOD from reg source
  RCV reg        -> (_ /\ to) <$> Just (execReceive from reg)
  JGZ cond jump  -> (_ /\ to) <$> execJGZ from cond jump

changeState :: ProgramState2 -> State2 -> State2
changeState to (State2 state) = State2 state { programState = to
                                             }

execReceive :: State2 -> Reg -> State2
execReceive state@(State2 {receiveQueue}) reg = case uncons receiveQueue of
  Just {head, tail} -> let
    (State2 updated) = setRegisterValue state reg head
    in moveForward $ State2 updated { receiveQueue = tail
                                    , programState = Running
                                    }
  Nothing -> changeState Waiting state

execRecover :: State -> Reg -> Maybe ProgramState
execRecover state reg = do
  x <- fetchRegisterValue state reg
  pure $ if x == 0.0
         then Continue $ moveForward state
         else Terminate state

execSend :: State2 -> State2 -> Reg -> Maybe (State2 /\ State2)
execSend (state@State2 from) (State2 to) reg = do
  frequency <- fetchRegisterValue state reg
  pure $ moveForward state /\ wrap to { receiveQueue = snoc to.receiveQueue frequency
                                      , receiveHistory = snoc to.receiveHistory frequency
                                      }

execSound :: State -> Reg -> Maybe State
execSound (state@State sta) reg = do
  frequency <- fetchRegisterValue state reg
  pure $ wrap sta { lastSoundFrequency = frequency
                  }

execSET :: forall a. HasUpdatableMemory a => a -> Reg -> Source -> Maybe a
execSET state reg = case _ of
  (Literal i) -> pure $ setRegisterValue state reg (toNumber i)
  (Register sourceReg) ->
    setRegisterValue state reg <$> fetchRegisterValue state sourceReg

execADD :: forall a. HasUpdatableMemory a => a -> Reg -> Source -> Maybe a
execADD = execBinary (+)

execMUL :: forall a. HasUpdatableMemory a => a -> Reg -> Source -> Maybe a
execMUL = execBinary (*)

execMOD :: forall a. HasUpdatableMemory a => a -> Reg -> Source -> Maybe a
execMOD = execBinary (flip remainder)

execJGZ :: forall a. HasMemory a => Movable a => a -> Source -> Source -> Maybe a
execJGZ state (Literal j) (Literal x) = pure $ if j <= 0
                                               then moveForward state
                                               else moveBy x state
execJGZ state (Literal j) (Register rJump) = if j <= 0
                                             then pure $ moveForward state
                                             else do
    x    <- fetchRegisterValue state rJump
    xInt <- fromNumber x
    pure $ moveBy xInt state
execJGZ state (Register rCond) (Literal x) = do
    rc <- fetchRegisterValue state rCond
    pure $ if rc <= 0.0
           then moveForward state
           else moveBy x state
execJGZ state (Register rCond) (Register rJump) = do
    rc    <- fetchRegisterValue state rCond
    rj    <- fetchRegisterValue state rJump
    rjInt <- fromNumber rj
    pure $ if rc <= 0.0
           then moveForward state
           else moveBy rjInt state

execBinary :: forall a. HasUpdatableMemory a => (Number -> Number -> Number) -> a -> Reg -> Source -> Maybe a
execBinary f state reg = case _ of
  (Literal i) -> do
    r1 <- fetchRegisterValue state reg
    pure $ setRegisterValue state reg (f (toNumber i) r1)
  (Register sourceReg) -> do
    r1 <- fetchRegisterValue state sourceReg
    r2 <- fetchRegisterValue state reg
    pure $ setRegisterValue state reg (f r1 r2)

moveForward :: forall a. Movable a => a -> a
moveForward = moveBy 1

moveBy :: forall a. Movable a => Int -> a -> a
moveBy = move
