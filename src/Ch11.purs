module Ch11 where

import Data.List (List(..), (:), singleton)
import Data.List.Types (NonEmptyList(..))
import Data.Foldable (class Foldable, foldl, foldr, foldMap)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))
import Data.Semiring (class Semiring, zero)
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Ord, Unit, show, negate, discard, otherwise, type (~>), ($), (>), (+), (<>), (<<<))

reverse :: List ~> List
reverse = foldl (\rl x -> x : rl) Nil

max :: ∀ a. Ord a => a -> a -> a
max x y
  | x > y = x
  | otherwise = y

findMax :: ∀ a. Ord a => List a -> Maybe a
findMax Nil = Nothing
findMax l@(first : _) = Just $ foldl max first l

findMaxNE :: ∀ a. Ord a => NonEmptyList a -> a
findMaxNE (NonEmptyList ne) = foldl1 max ne

foldl1 :: ∀ f a. Foldable f => (a -> a -> a) -> NonEmpty f a -> a
foldl1 f (x :| xs) = foldl f x xs

sum :: ∀ f a. Foldable f => Semiring a => f a -> a
sum = foldl (+) zero

data Tree a = Leaf a | Node (Tree a) (Tree a)

class ToList f where
  toList :: ∀ a. f a -> List a

newtype RightFirstTree a = RightFirstTree (Tree a)
newtype LeftFirstTree a = LeftFirstTree (Tree a)

instance toListRightFirstTree :: ToList RightFirstTree where
  toList (RightFirstTree (Leaf x)) = singleton x
  toList (RightFirstTree (Node lt rt)) = toList (RightFirstTree rt) <> toList (RightFirstTree lt)

instance toListLeftFirstTree :: ToList LeftFirstTree where
  toList (LeftFirstTree (Leaf x)) = singleton x
  toList (LeftFirstTree (Node lt rt)) = toList (LeftFirstTree lt) <> toList (LeftFirstTree rt)

instance foldableRightFirstTree :: Foldable RightFirstTree where
  foldr f acc = foldr f acc <<< toList
  foldl f acc = foldl f acc <<< toList
  foldMap f = foldMap f <<< toList

instance foldableLeftFirstTree :: Foldable LeftFirstTree where
  foldr f acc = foldr f acc <<< toList
  foldl f acc = foldl f acc <<< toList
  foldMap f = foldMap f <<< toList

test :: Effect Unit
test =
  do
    log $ show $ reverse (10 : 20 : 30 : Nil)
    log $ show $ max (-1) 99
    log $ show $ max "aa" "z"
    log $ show $ findMax (37 : 311 : -1 : 2 : 84 : Nil)
    log $ show $ findMax ("a" : "bbb" : "c" : Nil)
    log $ show $ findMaxNE (NonEmptyList $ 37 :| (311 : -1 : 2 : 84 : Nil))
    log $ show $ findMaxNE (NonEmptyList $ "a" :| ("bbb" : "c" : Nil))
    log $ show $ sum (1.0 : 2.0 : 3.0 : Nil)
    log $ show $ toList $ LeftFirstTree (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))
    log $ show $ sum $ LeftFirstTree (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))
    log $ show $ toList $ RightFirstTree (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))
    log $ show $ sum $ RightFirstTree (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))
