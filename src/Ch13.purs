module Ch13 where

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Show, Unit, show, discard, ($), (/))

data Maybe a = Nothing | Just a

derive instance genericMaybe :: Generic (Maybe a) _

instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

class Functor f where
  map :: ∀ a b. (a -> b) -> f a -> f b

instance functorMaybe :: Functor Maybe where
  map _ Nothing = Nothing
  map f (Just x) = Just $ f x

infixl 4 map as <$>

data Either a b = Left a | Right b

derive instance genericEither :: Generic (Either a b) _

instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow

instance functorEither :: Functor (Either a) where
  map _ (Left err) = Left err
  map f (Right x) = Right $ f x

test :: Effect Unit
test = do
  log $ show $ (_ / 2) <$> Just 10
  log $ show $ (_ / 2) <$> Nothing
  log $ show $ (_ / 2) <$> (Right 10 :: Either Unit _)
  log $ show $ (_ / 2) <$> Left "error reason"
