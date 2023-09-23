module Ch6 where

type Address =
  { street1 :: String
  , street2 :: String
  , city :: String
  , state :: String
  , zip :: String
  }

data Person = Person
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

