module Year2017.Day16 where

-- http://adventofcode.com/2017/day/16

import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Monad (class Monad)
import Control.Monad.Eff (Eff)
import Control.Monad.Rec.Class (Step(Loop, Done), tailRecM)
import Control.Monad.ST (ST, pureST)
import Data.Array (elemIndex, length, take, (..))
import Data.Array.ST (STArray, freeze, pokeSTArray, pushAllSTArray, spliceSTArray, thaw, unsafeFreeze)
import Data.Array.ST.Partial as Partial
import Data.Char (fromCharCode, toCharCode)
import Data.Either (Either(..))
import Data.Field (mod)
import Data.Function (id)
import Data.Function.Uncurried (Fn2, Fn3, Fn5, mkFn2, mkFn3, mkFn5, runFn2, runFn3, runFn5)
import Data.Functor (map)
import Data.List (List(..), (:))
import Data.Maybe (maybe)
import Data.String (fromCharArray)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unit (Unit, unit)
import Partial.Unsafe (unsafePartial)
import Prelude (bind, ($), ($>), (*), (*>), (+), (-), (/), (<$>), (<*>), (==))
import Text.Parsing.StringParser (ParseError, Parser, runParser)
import Text.Parsing.StringParser.Combinators (sepBy)
import Text.Parsing.StringParser.String (anyChar, char, string)
import Year2017.Day16.Types (Dance(..), LoopDetector(..))
import Year2017.Day8 (pIntPositive)

type Promenade = Array Char

parse :: String -> Either ParseError (List Dance)
parse = runParser pDance

pDance :: Parser (List Dance)
pDance = (pSpin <|> pExchange <|> pPartner) `sepBy` char ','

pSpin :: Parser Dance
pSpin = do
  _ <- string "s"
  p <- pIntPositive
  pure $ Spin p

pExchange :: Parser Dance
pExchange = do
  _ <- string "x"
  p <- pIntPositive
  _ <- string "/"
  r <- pIntPositive
  pure $ Exchange p r

pPartner :: Parser Dance
pPartner = do
  _ <- string "p"
  p <- anyChar
  _ <- string "/"
  r <- anyChar
  pure $ Partner p r

solution1 :: Promenade -> String -> String
solution1 promenade input = case parse input of
  Right danceSteps -> fromCharArray $ singleDance danceSteps promenade
  Left _ -> "parse error"

oneBilion :: Int
oneBilion = 1000000000

solution2 :: Promenade -> String -> String
solution2 promenade input = case parse input of
  Right danceSteps -> let
    loopSize = fst $ repeatDance oneBilion danceSteps promenade

    requiredIterations = oneBilion `mod` loopSize

    in fromCharArray $ snd $ repeatDance requiredIterations danceSteps promenade
  Left _ -> "parse error"

promenadeAE :: Promenade
promenadeAE = map fromCharCode (toCharCode 'a' .. toCharCode 'e')

promenadeAP :: Promenade
promenadeAP = map fromCharCode (toCharCode 'a' .. toCharCode 'p')

singleDance :: List Dance -> Promenade -> Promenade
singleDance ds promenade = pureST do
  arr <- thaw promenade
  runFn3 letsDance promenade ds arr *> freeze arr

repeatDance :: Int -> List Dance -> Promenade -> Int /\ Promenade
repeatDance iter ds promenade = pureST do
  arr      <- thaw promenade
  loopSize <- runFn5 runLoop 0 promenade iter ds arr
  arrUpd   <- freeze arr
  pure $ loopSize /\ arrUpd

type LoopData = { dance              :: List Dance
                , requiredIterations :: Int
                , loopSize           :: Int
                }

runLoop :: forall h r. Fn5 Int Promenade Int (List Dance) (STArray h Char) (Eff (st :: ST h | r) Int)
runLoop = mkFn5 \loopSize initial iter xs promenade -> tailRecM (go initial promenade) {dance: xs, requiredIterations: iter, loopSize} where
  go :: Promenade -> (STArray h Char) -> LoopData -> Eff (st :: ST h | r) (Step LoopData Int)
  go initial promenade {dance, requiredIterations: 0, loopSize} = pure (Done loopSize)
  go initial promenade {dance, requiredIterations, loopSize}    = do
           loopDetector <- runFn3 letsDance initial dance promenade
           pure $ if loopDetector == Terminate
                  then Done (loopSize+1)
                  else Loop { dance, requiredIterations: requiredIterations - 1, loopSize: loopSize + 1 }

letsDance :: forall h r. Fn3 Promenade (List Dance) (STArray h Char) (Eff (st :: ST h | r) LoopDetector)
letsDance = mkFn3 \initial ds promenade -> tailRecM (go initial promenade) ds where

  go :: Promenade -> STArray h Char -> List Dance -> Eff (st :: ST h | r) (Step (List Dance) LoopDetector)
  go _       promenade Nil   = pure (Done Continue)
  go initial promenade (h:t) = let

    loopEncountered :: Eff (st :: ST h | r) Boolean
    loopEncountered = map (_ == initial) (unsafeFreeze promenade)

    nextIteration :: Eff (st :: ST h | r) (Step (List Dance) LoopDetector)
    nextIteration = (runFn2 dance h promenade) $> Loop t

    in ifAfter nextIteration loopEncountered (pure $ Done Terminate)

ifAfter :: forall m a. Monad m => m a -> m Boolean -> m a -> m a
ifAfter effect mCondition whenCondition = do
  a         <- effect
  condition <- mCondition
  if condition then whenCondition else pure a

dance :: forall h r. Fn2 Dance (STArray h Char) (Eff (st :: ST h | r) Unit)
dance = mkFn2 \a arr -> case a of
 (Spin i)       -> runFn2 makeSpin i arr
 (Exchange i j) -> unsafePartial $ runFn3 makeExchange i j arr
 (Partner a b)  -> runFn3 makePartner a b arr

makeSpin :: forall h r. Fn2 Int (STArray h Char) (Eff (st :: ST h | r) Unit)
makeSpin = mkFn2 \i arr -> do
   arrImmut <- freeze arr
   let len       = (length arrImmut) - i
       toAppend  = take len arrImmut
   spliceSTArray arr 0 len [] *>
   pushAllSTArray arr toAppend $>
   unit

makeExchange :: forall h r. Partial => Fn3 Int Int (STArray h Char) (Eff (st :: ST h | r) Unit)
makeExchange = mkFn3 \i j arr -> do
  first  <- Partial.peekSTArray arr i
  second <- Partial.peekSTArray arr j
  pokeSTArray arr i second *>
  pokeSTArray arr j first $>
  unit

makePartner :: forall h r. Fn3 Char Char (STArray h Char) (Eff (st :: ST h | r) Unit)
makePartner = mkFn3 \a b arr -> do
  arrImmut <- unsafeFreeze arr
  let first /\ second = maybe (0 /\ 0) id ((/\) <$> (elemIndex a arrImmut) <*> (elemIndex b arrImmut))
  pokeSTArray arr first b *>
  pokeSTArray arr second a $>
  unit
