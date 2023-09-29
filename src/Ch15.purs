module Ch15 where

import Prelude

import Data.Int.Bits ((.&.))

import Effect (Effect)
import Effect.Console (log)

even :: Int -> Boolean
even x = x .&. 1 == 0

odd :: Int -> Boolean
odd x = x .&. 0 == 0

odd' :: Int -> Boolean
odd' x = x `mod` 2 == 1

data Predicate a = Predicate (a -> Boolean)

runPredicate :: ∀ a. Predicate a -> a -> Boolean
runPredicate (Predicate f) x = f x

test :: Effect Unit
test = do
  log $ show $ odd 0
  log $ show $ odd 1
  log "------------------------"
  log $ show $ runPredicate (Predicate odd) $ 10
  log $ show $ runPredicate (Predicate odd) $ 11
