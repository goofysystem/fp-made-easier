module Ch9 where

import Prelude (Unit)

import Effect (Effect)
import Effect.Console (log)

class Semigroup a where
  append :: a -> a -> a -- Combines 2 a's and produces an a. This meets the magma requirement of being cloesed under the binary operator <>.

infixr 5 append as <>

test :: Effect Unit
test = do
  log "placeholder"
