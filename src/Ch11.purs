module Ch11 where

import Data.List (List(..), (:), foldl)
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Ord, Unit, show, negate, discard, otherwise, type (~>), ($), (>))

reverse :: List ~> List
reverse = foldl (\rl x -> x : rl) Nil

max :: ∀ a. Ord a => a -> a -> a
max x y
  | x > y = x
  | otherwise = y

findMax :: ∀ a. Ord a => a -> List a -> a
findMax mx Nil = mx
findMax mx (x : xs) = findMax (max x mx) xs

test :: Effect Unit
test = do
  log $ show $ reverse (10 : 20 : 30 : Nil)
  log $ show $ max (-1) 99
  log $ show $ max "aa" "z"
  log $ show $ findMax 0 (37 : 311 : -1 : 2 : 84 : Nil)
  log $ show $ findMax "" ("a" : "bbb" : "c" : Nil)
