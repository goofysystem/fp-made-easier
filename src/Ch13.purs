module Ch13 where

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Eq (class Eq)
import Data.String.Common (toUpper)
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Show, Unit, show, discard, identity, ($), (/), (<>), (==), (<<<), (*))

data Maybe a = Nothing | Just a

derive instance genericMaybe :: Generic (Maybe a) _

instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

class Functor f where
  map :: ∀ a b. (a -> b) -> f a -> f b

instance functorMaybe :: Functor Maybe where
  map _ Nothing = Nothing
  map f (Just x) = Just $ f x

derive instance eqMaybe :: Eq a => Eq (Maybe a)

infixl 4 map as <$>

data Either a b = Left a | Right b

derive instance genericEither :: Generic (Either a b) _

instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow

instance functorEither :: Functor (Either a) where
  map _ (Left err) = Left err
  map f (Right x) = Right $ f x

data Tuple a b = Tuple a b

derive instance genericTuple :: Generic (Tuple a b) _

instance showTuple :: (Show a, Show b) => Show (Tuple a b) where
  show = genericShow

instance functorTuple :: Functor (Tuple a) where
  map f (Tuple x y) = Tuple x $ f y

data Threeple a b c = Threeple a b c

derive instance genericThreeple :: Generic (Threeple a b c) _
instance showThreeple :: (Show a, Show b, Show c) => Show (Threeple a b c) where
  show = genericShow

instance functorThreeple :: Functor (Threeple a b) where
  map f (Threeple x y z) = Threeple x y $ f z

class Bifunctor f where
  bimap :: ∀ a b c d. (a -> c) -> (b -> d) -> f a b -> f c d

rmap :: ∀ f a b c. Bifunctor f => (b -> c) -> f a b -> f a c
rmap = bimap identity

lmap :: ∀ f a b c. Bifunctor f => (a -> c) -> f a b -> f c b
lmap f = bimap f identity

instance bifunctorEither :: Bifunctor Either where
  bimap f _ (Left err) = Left $ f err
  bimap _ g (Right x) = Right $ g x

test :: Effect Unit
test = do
  log $ show $ (_ / 2) <$> Just 10
  log $ show $ (_ / 2) <$> Nothing
  log $ show $ (_ / 2) <$> (Right 10 :: Either Unit _)
  log $ show $ (_ / 2) <$> Left "error reason"
  log $ show $ (_ / 2) <$> Tuple 10 20
  log $ show $ (_ / 2) <$> Threeple 10 20 20
  log $ show $ "Maybe Identity for Nothing: " <> show ((identity <$> Nothing) == (Nothing :: Maybe Unit))
  let
    g x = x * 2
    f x = x * 3
  log $ show $ "Maybe Composition for Nothing: " <> show ((map (g <<< f) Nothing) == (map f <<< map g) Nothing)
  log $ show $ "Maybe Composition for Just: " <> show ((map (g <<< f) (Just 60)) == (map f <<< map g) (Just 60))
  log $ show $ rmap (_ * 2) $ Left "error reason"
  log $ show $ rmap (_ * 2) $ (Right 10 :: Either Unit _)
  log $ show $ lmap toUpper $ (Left "error reason" :: Either _ Unit)
  log $ show $ lmap toUpper $ Right 10
