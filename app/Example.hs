{-# LANGUAGE RecordWildCards #-}

module Example where

import Data.Maybe

import CM.Metamodel
import CM.Visualization

data Address = Address { addressStreet :: String
                       , addressCity :: String
                       , addressPostcode :: Int
                       , addressCountry :: String
                       }
             deriving (Show, Read)

instance Identifiable Address

instance CMElement Address where
  toMeta = toMetaEntity

instance Entity Address where
  entityAttributes Address {..} =
    map tupleToAttribute
      [ ("street", "String", addressStreet)
      , ("city", "String", addressCity)
      , ("postcode", "Int", show addressPostcode)
      , ("country", "String", addressCountry)
      ]

data Neighbors = Neighbors { leftNeighbor :: Address
                           , rightNeighbor :: Address
                           }
               deriving (Show, Read)

instance Identifiable Neighbors

instance CMElement Neighbors where
  toMeta = toMetaRelationship

instance Relationship Neighbors where
  relationshipParticipations Neighbors {..} =
    map tupleToParticipation
      [ ("left", "Address", identifier leftNeighbor, Optional Unlimited)
      , ("right", "Address", identifier rightNeighbor, Optional Unlimited)
      ]

addr1 = Address { addressStreet = "A"
                , addressCity = ""
                , addressPostcode = 0
                , addressCountry = ""
                }

addr2 = Address { addressStreet = "B"
                , addressCity = ""
                , addressPostcode = 0
                , addressCountry = ""
                }

rel1 = Neighbors { leftNeighbor = addr1
                 , rightNeighbor = addr2
                 }

data ExampleModel = ExampleModel { mAddresses :: [Address]
                                 , mNeighbors :: [Neighbors]
                                 }
                  deriving (Show, Read)

instance CMElement ExampleModel where
  toMeta = toMetaModel

instance ConceptualModel ExampleModel where
  cmodelElements m = (map (toMeta m) $ mAddresses m) ++ (map (toMeta m) $ mNeighbors m)

model = ExampleModel { mAddresses = [addr1, addr2]
                     , mNeighbors = [rel1]
                     }
