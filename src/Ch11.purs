module Ch11 where

import Data.List (List(..), (:), foldl)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, show, type (~>), ($))

reverse :: List ~> List
reverse = foldl (\rl x -> x : rl) Nil

test :: Effect Unit
test = do
  log $ show $ reverse (10 : 20 : 30 : Nil)
