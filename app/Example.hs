{-# LANGUAGE RecordWildCards #-}
module Example where

import Data.Maybe

import CM.Metamodel
import CM.Visualization

data Address = Address {
  addressStreet :: String,
  addressCity :: String,
  addressPostcode :: Int,
  addressCountry :: String
} deriving (Show, Read)

instance Identifiable Address

instance CMElement Address where
  toMeta addr = Just MetaEntity { meName = "Address", meAttributes = entityAttributes addr, meIdentifier = identifier addr, meValid = valid addr}

instance Entity Address where
  entityAttributes Address { .. } = map tupleToAttribute [("street", "String", addressStreet),("city", "String", addressCity),("postcode", "Int", show addressPostcode),("country", "String", addressCountry)]

data Neighbors = Neighbors {
  leftNeighbor :: Address,
  rightNeighbor :: Address
} deriving (Show, Read)

instance Identifiable Neighbors

instance CMElement Neighbors where
  toMeta n = Just MetaRelationship { mrName = "Neighbors", mrParticipations = relationshipParticipations n, mrIdentifier = identifier n, mrValid = valid n}

instance Relationship Neighbors where
  relationshipParticipations Neighbors { .. } = map tupleToParticipation [("left", "Address", identifier leftNeighbor, Optional Unlimited),
                                                                          ("right", "Address", identifier rightNeighbor, Optional Unlimited)]

addr1 = Address {addressStreet="A", addressCity="", addressPostcode=0, addressCountry=""}
addr2 = Address {addressStreet="B", addressCity="", addressPostcode=0, addressCountry=""}
rel1 = Neighbors {leftNeighbor = addr1, rightNeighbor = addr2}
model = MetaModel {mmName = Just "TestModel", mmElements = (mapMaybe toMeta [addr1, addr2]) ++ (mapMaybe toMeta [rel1]), mmIdentifier = Just "yolo", mmValid = True}
