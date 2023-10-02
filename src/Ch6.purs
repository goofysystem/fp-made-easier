module Ch6 where

import Data.Eq
import Data.Newtype (class Newtype, unwrap)
import Prelude (($))

type Address =
  { street1 :: String
  , street2 :: String
  , city :: String
  , state :: String
  , zip :: String
  }

newtype Person = Person
  { name :: String
  , age :: Int
  , address :: Address
  }

data Company = Company
  { name :: String
  , address :: Address
  }

data EmptyLot = EmptyLot
  { daysEmpty :: Int
  , price :: Int
  , address :: Address
  }

data Residence = Home Address | Facility Address

class HasAddress a where
  getAddress :: a -> Address

instance hasAddressPerson :: HasAddress Person where
  getAddress (Person p) = p.address

instance hasAddressCompany :: HasAddress Company where
  getAddress (Company c) = c.address

instance hasAddressResidence :: HasAddress Residence where
  getAddress (Home a) = a
  getAddress (Facility a) = a

instance hasAddressEmptyLot :: HasAddress EmptyLot where
  getAddress (EmptyLot l) = l.address

newtype FirstName = FirstName String

derive instance newTypeFirstName :: Newtype FirstName _
derive instance eqFirstName :: Eq FirstName

newtype Ceo = Ceo Person

derive instance newtypeCeo :: Newtype Ceo _

newtype Janitor = Janitor Person

derive instance newtypeJanitor :: Newtype Janitor _

genericPersonHasAddress :: ∀ a. Newtype a Person => a -> Address
genericPersonHasAddress wrappedPerson =
  getAddress $ unwrap wrappedPerson

instance hasAddressCeo :: HasAddress Ceo where
  getAddress = genericPersonHasAddress

instance hasAddressJanitor :: HasAddress Janitor where
  getAddress = genericPersonHasAddress
