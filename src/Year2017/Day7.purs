module Year2017.Day7 where

-- http://adventofcode.com/2017/day/7

import Control.Applicative (pure)
import Data.Array (filter, last, length, nub, reverse, sortWith, uncons)
import Data.Either (Either(..), either)
import Data.Eq (class Eq)
import Data.Foldable (foldMap, foldl, foldr)
import Data.Functor (map)
import Data.Generic.Rep as Generic
import Data.Generic.Rep.Eq as Generic.Eq
import Data.Generic.Rep.Show as Generic.Show
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype, over)
import Data.Semigroup (class Semigroup)
import Data.Show (class Show)
import Data.StrMap (member, values)
import Data.StrMap as M
import Data.Traversable (sequence)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unit (Unit, unit)
import Prelude (bind, id, not, ($), (+), (-), (<<<), (<>), (==), (>>=))

type Name   = String
type Weight = Int

data Tree a   = Node Name a (Array (Tree a)) | Leaf Name a
data TreeData = TData Name Weight (Array Name)

type Unbalanced = Array (Name /\ Weight /\ Array (Name /\ Weight))
newtype Answer  = Answer (Either Unit Unbalanced)

derive instance genericTreeData :: Generic.Generic TreeData _

instance showTreeData :: Show TreeData where
  show = Generic.Show.genericShow

derive instance myAnswer :: Generic.Generic Answer _

instance showAnswer :: Show Answer where
  show = Generic.Show.genericShow

instance eqAnswer :: Eq Answer where
  eq = Generic.Eq.genericEq

derive instance newtypeAnswer :: Newtype Answer _

instance semigroupAnswer :: Semigroup Answer where
  append (Answer (Right a)) (Answer (Right b)) = Answer (Right (a <> b))
  append (Answer (Right a)) _                  = Answer (Right a)
  append _ (Answer (Right b))                  = Answer (Right b)
  append l1 l2                                 = l1

instance monoidAnswer :: Monoid Answer where
  mempty = Answer (Left unit)

getName :: forall a. Tree a -> Name
getName (Leaf n _  ) = n
getName (Node n _ _) = n

findSolution :: Unbalanced -> Maybe (Name /\ Int)
findSolution xs = diff where

  countMe :: Array (Name /\ Weight) -> Weight
  countMe xs = foldl (+) 0 $ map snd xs

  nameAndCount :: Name /\ Weight /\ Array (Name /\ Weight) -> Name /\ Weight /\ Int
  nameAndCount (name /\ w /\ xs) = name /\ w /\ (w + countMe xs)

  agg :: Array (Name /\ Weight /\ Int)
  agg = reverse $ sortWith (snd <<< snd) $ map nameAndCount xs

  diff :: Maybe (Name /\ Int)
  diff = do
    { head: name /\ w /\ t, tail: t1 } <- uncons agg
    { head: _    /\ _ /\ s, tail: _  } <- uncons t1
    pure $ name /\ (w - (t - s))

treeDataToTree :: Array TreeData -> Maybe (Tree Weight)
treeDataToTree treeData = last $ values (constructTree treeData)

solution :: Array TreeData -> Maybe (Name /\ Int)
solution treeData = (toUnbalanced treeData) >>= \(Answer xs) -> either (\_ -> Nothing) findSolution xs

toUnbalanced :: Array TreeData -> Maybe Answer
toUnbalanced treeData = map (getUnbalance <<< Left) (treeDataToTree treeData)

getUnbalance :: Either (Tree Weight) Unbalanced -> Answer
getUnbalance (Right xs) = Answer $ Right xs
getUnbalance (Left (Leaf name w)) = Answer $ Left unit
getUnbalance (Left (Node name w xs)) = unbalanced where
  distinct :: Int
  distinct = length $ nub $ map getWeight xs

  currentUnbalance :: Answer
  currentUnbalance = foldMap (getUnbalance <<< Left) xs

  unbalanceStep :: Either Unit Unbalanced -> Unbalanced
  unbalanceStep = either (\_ -> map getChildInfo xs) id

  unbalanced :: Answer
  unbalanced = if 1 == distinct then currentUnbalance else over Answer (Right <<< unbalanceStep) currentUnbalance

getChildInfo :: Tree Weight -> Name /\ Weight /\ Array (Name /\ Weight)
getChildInfo (Leaf name weight)    = name /\ weight /\ []
getChildInfo (Node name weight xs) = name /\ weight /\ map (\x -> getName x /\ getWeight x) xs

getWeight :: (Tree Weight) -> Int
getWeight (Leaf _ w) = w
getWeight (Node _ weight xs) = weight + foldl (+) 0 (map getWeight xs)

constructTree :: Array TreeData -> M.StrMap (Tree Weight)
constructTree treeData = prepareTree treeData M.empty

prepareTree :: Array TreeData -> M.StrMap (Tree Weight) -> M.StrMap (Tree Weight)
prepareTree treeData acc = let
  treesMap = foldr processTreeData acc treeData
  unprocessTreeData = filter (\(TData name _ _) -> not (member name treesMap)) treeData
  in case unprocessTreeData of
    [] -> treesMap
    xs -> prepareTree unprocessTreeData treesMap

processTreeData :: TreeData -> M.StrMap (Tree Weight) -> M.StrMap (Tree Weight)
processTreeData (TData name weight []) acc = M.insert name (Leaf name weight) acc
processTreeData (TData name weight xs) acc = let
  branches     = sequence $ map (\name -> M.lookup name acc) xs
  insertNode n = M.insert name (Node name weight n) acc
  in maybe acc insertNode branches
