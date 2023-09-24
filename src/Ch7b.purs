module Ch7b where

import Prelude

import Effect (Effect)
import Effect.Console (log)

newtype CSV = CSV String

class ToCSV a where
  toCSV :: a -> CSV

test :: Effect Unit
test = do
  log "placeholder"
