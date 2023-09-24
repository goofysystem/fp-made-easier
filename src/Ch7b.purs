module Ch7b where

import Prelude

import Data.Newtype (class Newtype)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

import Effect (Effect)
import Effect.Console (log)

newtype CSV = CSV String

class ToCSV a where
  toCSV :: a -> CSV

newtype FullName = FullName String

instance showFullName :: Show FullName where
  show (FullName name) = name

newtype Age = Age Int

derive instance newTypeAge :: Newtype Age _
derive newtype instance showAge :: Show Age

data Occupation = Doctor | Dentist | Lawyer | Unemployed

derive instance genericOccupation :: Generic Occupation _

instance showOccupation :: Show Occupation where
  show = genericShow

data Person = Person
  { name :: FullName
  , age :: Age
  , occupation :: Occupation
  }

instance toCSVPerson :: ToCSV Person where
  toCSV (Person { name, age, occupation }) = CSV $ show name <> "," <> show age <> "," <> show occupation

test :: Effect Unit
test = do
  log "placeholder"
