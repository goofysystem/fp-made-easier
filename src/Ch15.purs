module Ch15 where

import Prelude

import Data.Int.Bits ((.&.))
import Data.Functor.Contravariant (class Contravariant, cmap, (>$<))
import Data.Profunctor (class Profunctor)
import Data.Foldable (class Foldable, foldl)
import Data.List (List(..), (:))

import Effect (Effect)
import Effect.Console (log)

even :: Int -> Boolean
even x = x .&. 1 == 0

odd :: Int -> Boolean
odd x = x .&. 0 == 0

odd' :: Int -> Boolean
odd' x = x `mod` 2 == 1

data Predicate a = Predicate (a -> Boolean)

instance contravariantPredicate :: Contravariant Predicate where
  cmap f (Predicate g) = Predicate (g <<< f)

runPredicate :: ∀ a. Predicate a -> a -> Boolean
runPredicate (Predicate f) x = f x

data Moore s a b = Moore s (s -> b) (s -> a -> s)

instance profunctorMoore :: Profunctor (Moore s) where
  dimap :: ∀ a b c d. (c -> a) -> (b -> d) -> Moore s a b -> Moore s c d
  dimap f g (Moore s0 output transition) = Moore s0 (g <<< output) (\s -> transition s <<< f)

addr :: ∀ a. Semiring a => Moore a a a
addr = Moore zero identity (+)

runFoldL :: ∀ s a b f. Foldable f => Moore s a b -> f a -> b
-- runFoldL (Moore s0 output transition) xs = output $ foldl transition s0 xs
runFoldL (Moore s0 output transition) = output <<< foldl transition s0

test :: Effect Unit
test = do
  log $ show $ odd 0
  log $ show $ odd 1
  log "------------------------"
  log $ show $ runPredicate (Predicate odd) $ 10
  log $ show $ runPredicate (Predicate odd) $ 11
  log "------------------------"
  log $ show $ runPredicate (cmap (_ + 1) (Predicate odd)) 10
  log $ show $ runPredicate (cmap (_ + 2) (Predicate odd)) 10
  log $ show $ runPredicate ((_ + 1) >$< (Predicate odd)) 10
  log $ show $ runPredicate ((_ + 2) >$< (Predicate odd)) 10
  log "------------------------"
  log $ show $ runFoldL addr [1, 2, 3]
  log $ show $ runFoldL addr (1.0 : 2.0 : 3.0 : Nil)
