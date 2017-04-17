module Example where

data Person = Person {
  firstname :: String,
  lastname :: String,
  personalId :: String,
  address :: String
}

data CarType = CarType {
  brand :: String,
  model :: String,
  yearManufactures :: Int,
  owner :: Person
}

markCar :: CarType -> Int
markCar c = if brand c == "Škoda" then 1 else 0
