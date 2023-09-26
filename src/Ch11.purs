module Ch11 where

import Data.List (List(..), (:), foldl)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Ord, Unit, show, negate, discard, otherwise, type (~>), ($), (>))

reverse :: List ~> List
reverse = foldl (\rl x -> x : rl) Nil

max :: ∀ a. Ord a => a -> a -> a
max x y
  | x > y = x
  | otherwise = y

findMax :: ∀ a. Ord a => List a -> Maybe a
findMax Nil = Nothing
findMax l@(first : _) = Just $ foldl max first l

test :: Effect Unit
test = do
  log $ show $ reverse (10 : 20 : 30 : Nil)
  log $ show $ max (-1) 99
  log $ show $ max "aa" "z"
  log $ show $ findMax (37 : 311 : -1 : 2 : 84 : Nil)
  log $ show $ findMax ("a" : "bbb" : "c" : Nil)
