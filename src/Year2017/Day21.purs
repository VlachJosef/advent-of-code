module Year2017.Day21 where

-- http://adventofcode.com/2017/day/21

import Control.Alt ((<|>))
import Control.Applicative (pure)
import Data.Array (drop, fromFoldable, head, length, nub, reverse, singleton, snoc, sort, take, zipWith, (..))
import Data.Array.Partial as P
import Data.Either (Either)
import Data.Eq (class Eq)
import Data.Field (mod)
import Data.Foldable (class Foldable, foldMap, foldl, sum)
import Data.Function (flip, id)
import Data.Functor (class Functor, map)
import Data.List (List)
import Data.Map (lookup)
import Data.Map as Map
import Data.Maybe (maybe)
import Data.Newtype (unwrap, wrap)
import Data.Ord (class Ord)
import Data.Sequence.Internal ((<$$>))
import Data.String (trim)
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafePartial)
import Prelude (bind, const, ($), ($>), (-), (/), (<$>), (<*), (<<<), (<>), (=<<), (==))
import Text.Parsing.StringParser (ParseError, Parser, runParser)
import Text.Parsing.StringParser.Combinators (many1, sepBy)
import Text.Parsing.StringParser.String (char, string)
import Year2017.Day21.Types (Pattern(..), Pixel(..), RewriteRule(..), RuleLookup)

pPixel :: Parser Pixel
pPixel = pOn <|> pOff where
  pOn  = char '#' $> On
  pOff = char '.' $> Off

pPattern :: Parser Pattern
pPattern = wrap <$> fromFoldable <$> (fromFoldable <$> many1 pPixel) `sepBy` char '/'

pRewriteRule :: Parser RewriteRule
pRewriteRule = do
  from <- pPattern <* string " => "
  to   <- pPattern
  pure $ RewriteRule from to

pRewriteRules :: Parser (List RewriteRule)
pRewriteRules = pRewriteRule `sepBy` (char '\n')

allPossibleCombinations :: RewriteRule -> Array RewriteRule
allPossibleCombinations (RewriteRule from to) = flip RewriteRule to <<< wrap <$> allCombination (unwrap from) where

  allCombination :: forall a. Eq a => Ord a => Array (Array a) -> Array (Array (Array a))
  allCombination xs = sort $ nub $ flipCombination =<< rotateN xs <$> 0 .. 3

solution :: Int -> String -> Either ParseError Int
solution iter str = do
  ruleLookup <- toRuleLookup <<< (allPossibleCombinations =<< _) <<< fromFoldable <$> runParser pRewriteRules (trim str)
  pure $ countOns $ foldl (const <<< produceNextGeneration ruleLookup) initialPattern (drop 1 (0 .. iter))

initialPattern :: Pattern
initialPattern = wrap [[Off, On , Off]
                      ,[Off, Off, On ]
                      ,[On , On , On ]
                      ]

countOns :: Pattern -> Int
countOns (Pattern xs) = sum $ sum <$> map isOn <$> xs where

  isOn :: Pixel -> Int
  isOn On = 1
  isOn Off = 0

produceNextGeneration :: RuleLookup -> Pattern -> Pattern
produceNextGeneration rb (Pattern xs) = let

  replacePatterns :: Array Pattern -> Array Pattern
  replacePatterns xs = (\x -> maybe (Pattern []) id (lookup x rb)) <$> xs

  divisibleBy = if (length xs) `mod` 2 == 0 then 2 else 3

  toBeReplaced :: Array (Array Pattern)
  toBeReplaced = Pattern <$$> (splitArray divisibleBy xs)

  replaced :: Array (Array Pattern)
  replaced = replacePatterns <$> toBeReplaced

  in Pattern $ flattenArray $ (\(Pattern a) -> a) <$$> replaced

flattenArray :: forall a. Array (Array (Array (Array a))) -> Array (Array a)
flattenArray = foldMap (foldl joinArrays []) where

  joinArrays :: Array (Array a) -> Array (Array a) -> Array (Array a)
  joinArrays [] ys = ys
  joinArrays xs ys = zipWith (<>) xs ys

splitArray :: forall a. Int -> Array (Array a) -> Array (Array (Array (Array a)))
splitArray n xs = go xs [] where

  go [] acc = acc
  go xs acc = let
    restOfSegments  = drop n xs
    segmentData     = take n xs
    splittedSegment = splitSegment n segmentData
    in go restOfSegments (snoc acc splittedSegment)

splitSegment :: forall a. Int -> Array (Array a) -> Array (Array (Array a))
splitSegment i xs = go iter xs [] where

  iter = (maybe 0 id (length <$> head xs)) / i

  go 0 _  acc = acc
  go n ax acc = let
    h = take i <$> ax
    restOfSegments = drop i <$> ax
    in go (n - 1) restOfSegments (snoc acc h)

transpose :: forall a. Array (Array a) -> Array (Array a)
transpose p = unsafePartial $ let
  tr :: Array (Array (Array a))
  tr = transpose1D <$> p
  in foldl (zipWith (<>)) (P.head tr) (P.tail tr)

transpose1D :: forall a. Array a -> Array (Array a)
transpose1D xs = singleton <$> xs

flipVertical :: forall a. Array (Array a) -> Array (Array a)
flipVertical = reverse

rotate :: forall a. Array (Array a) -> Array (Array a)
rotate = transpose <<< flipVertical

flipHorizontal :: forall a. Array (Array a) -> Array (Array a)
flipHorizontal = rotate <<< transpose

rotateN :: forall a. Array (Array a) -> Int -> Array (Array a)
rotateN xs n = go n xs where
  go 0 acc = acc
  go n acc = go (n-1) (rotate acc)

flipCombination :: forall a. Array (Array a) -> Array (Array (Array a))
flipCombination xs = (_ $ xs) <$> [ flipHorizontal
                                  , flipHorizontal <<< flipVertical
                                  ]

toRuleLookup :: forall f. Foldable f => Functor f => f RewriteRule -> RuleLookup
toRuleLookup xs = Map.fromFoldable (toTuple <$> xs) where
  toTuple :: RewriteRule -> Pattern /\ Pattern
  toTuple (RewriteRule to from) = to /\ from
