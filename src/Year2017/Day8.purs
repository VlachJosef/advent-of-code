module Year2017.Day8 where

-- http://adventofcode.com/2017/day/8

import Control.Alt ((<|>))
import Control.Applicative (pure)
import Data.Array (fromFoldable)
import Data.Either (Either, either)
import Data.Field (negate)
import Data.Foldable (foldl,  maximum)
import Data.Functor (map)
import Data.Generic.Rep as Generic
import Data.Generic.Rep.Show as Generic.Show
import Data.Int (fromString)
import Data.Maybe (Maybe, maybe)
import Data.Show (class Show)
import Data.StrMap (lookup, values)
import Data.StrMap as M
import Data.String (fromCharArray)
import Data.Traversable (sequence)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Prelude (bind, discard, id, ($), ($>), (+), (-), (/=), (<), (<<<), (<=), (<>), (==), (>), (>=))
import Text.Parsing.StringParser (ParseError, Parser, fail, runParser)
import Text.Parsing.StringParser.Combinators (many1)
import Text.Parsing.StringParser.String (anyDigit, regex, skipSpaces, string)

type Instructions     = Array String
type Memory           = M.StrMap (Int /\ Int)

newtype Register      = Register String
newtype RegisterValue = RegisterValue Int

data Operation        = Inc | Dec
data Op               = LT | GT | LE | GE | EQ | NE
data Condition        = Condition Register Op RegisterValue
data Instruction      = Instruction Register Operation RegisterValue Condition

parseInstructions :: Instructions -> Either ParseError (Array Instruction)
parseInstructions insts = sequence $ map (runParser parseString) insts

run :: Instructions -> Memory
run instructions = either (\_ -> M.empty) evaluateProgram (parseInstructions instructions)

solution1 :: Instructions -> Maybe Int
solution1 = biggestValue <<< run

solution2 :: Instructions -> Maybe Int
solution2 = biggestValueHold <<< run

biggestValue :: Memory -> Maybe Int
biggestValue = projection snd

biggestValueHold :: Memory -> Maybe Int
biggestValueHold = projection fst

projection :: (Int /\ Int -> Int) -> Memory -> Maybe Int
projection f xs = maximum $ map f (values xs)

evaluateProgram :: (Array Instruction) -> Memory
evaluateProgram = foldl executeInstruction M.empty

executeInstruction :: Memory -> Instruction -> Memory
executeInstruction acc (Instruction reg op regVal condition) =
  if (conditionHold acc condition) then
    updateRegister op acc reg regVal
  else
    acc

updateRegister :: Operation -> Memory -> Register -> RegisterValue -> Memory
updateRegister op acc rn@(Register register) (RegisterValue delta) = let
  operation              = getOperation op
  (maxValue /\ oldValue) = getRegisterValue acc rn
  updated                = operation oldValue delta
  updatedMaxValue        = maybe 0 id $ maximum [maxValue, updated]
  in M.insert register (updatedMaxValue /\ updated) acc

getOperation :: Operation -> (Int -> Int -> Int)
getOperation Inc = (+)
getOperation Dec = (-)

conditionHold :: Memory -> Condition -> Boolean
conditionHold acc (Condition register condition (RegisterValue value)) = cond condition (snd (getRegisterValue acc register)) value

getRegisterValue :: Memory -> Register -> Int /\ Int
getRegisterValue acc (Register r) = maybe (0 /\ 0) id $ lookup r acc

cond :: Op -> Int -> Int -> Boolean
cond LE = (<=)
cond LT = (<)
cond GE = (>=)
cond GT = (>)
cond EQ = (==)
cond NE = (/=)

pOp :: Parser Op
pOp = string "<=" $> LE
  <|> string "<"  $> LT
  <|> string ">=" $> GE
  <|> string ">"  $> GT
  <|> string "==" $> EQ
  <|> string "!=" $> NE

pRegister :: Parser Register
pRegister = map Register (regex "\\w+")

pRegisterValue :: Parser RegisterValue
pRegisterValue = map RegisterValue pInt

pOperation :: Parser Operation
pOperation = (string "inc") $> Inc
         <|> (string "dec") $> Dec

pDigits :: Parser String
pDigits = map fromCharArray (map fromFoldable $ many1 anyDigit)

pIntNegative :: Parser Int
pIntNegative = map (\i -> -i) do
  _ <- string "-"
  pIntPositive

pIntPositive :: Parser Int
pIntPositive = do
  digits <- pDigits
  maybe (fail $ "Expected a number, got " <> digits) pure (fromString digits)

pInt :: Parser Int
pInt = pIntNegative <|> pIntPositive

parseString :: Parser Instruction
parseString = do
  register          <- pRegister
  skipSpaces
  operation         <- pOperation
  skipSpaces
  registerValue     <- pRegisterValue
  skipSpaces
  _                 <- string "if"
  skipSpaces
  condRegister      <- pRegister
  skipSpaces
  op                <- pOp
  skipSpaces
  condRegisterValue <- pRegisterValue
  pure $ Instruction register operation registerValue (Condition condRegister op condRegisterValue)

derive instance genericOperation :: Generic.Generic Operation _

instance showOperation :: Show Operation where
  show = Generic.Show.genericShow

derive instance genericOp :: Generic.Generic Op _

instance showOp :: Show Op where
  show = Generic.Show.genericShow

derive instance genericCondition :: Generic.Generic Condition _

instance showCondition :: Show Condition where
  show = Generic.Show.genericShow

derive instance genericRegisterValue :: Generic.Generic RegisterValue _

instance showRegisterValue :: Show RegisterValue where
  show = Generic.Show.genericShow

derive instance genericRegister :: Generic.Generic Register _

instance showRegister :: Show Register where
  show = Generic.Show.genericShow

derive instance genericInstruction :: Generic.Generic Instruction _

instance showInstruction :: Show Instruction where
  show = Generic.Show.genericShow
