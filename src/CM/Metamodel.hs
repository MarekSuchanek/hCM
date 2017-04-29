{-# LANGUAGE RecordWildCards #-}
module CM.Metamodel where

import Data.Maybe

type Identifier = String

class (Show i) => Identifiable i where
  identifier :: i -> Identifier
  identifier = show
  identic :: i -> i -> Bool
  identic x y = identifier x == identifier y

-- Generic element of conceptual model
class (Show e, Read e) => CMElement e where
  constraints :: [e -> Bool]
  constraints = []
  valid :: e -> Bool
  valid elem = all ($ elem) constraints
  elementName :: e -> String
  elementName = takeWhile (/= ' ') . show
  toMeta :: e -> Maybe MetaElement
  toMeta _ = Nothing

-- Entities for representing concepts
class (CMElement a, Identifiable a) => Entity a where
  entityAttributes :: a -> [MetaAttribute]
  entityName :: a -> String
  entityName = elementName
  entitySuperNames :: a -> [String]
  entitySuperNames _ = []

-- Relationship that connects multiple entities
class (CMElement a, Identifiable a) => Relationship a where
  relationshipParticipations :: a -> [MetaParticipation]
  relationshipName :: a -> String
  relationshipName = elementName
  relationshipSuperNames :: a -> [String]
  relationshipSuperNames _ = []

-- Whole conceptual model class
class (CMElement a) => ConceptualModel a where
  cmodelElements :: a -> [MetaElement]
  cmodelName :: a -> String
  cmodelName = elementName

--------------------------------------------------------------------------------
data ParticipationQuantity = Limited Word | Unlimited deriving (Show, Read, Eq, Ord)
data ParticipationType = MandatoryUnique | OptionalUnique
                       | Mandatory ParticipationQuantity  | Optional ParticipationQuantity
                       | Custom ParticipationQuantity ParticipationQuantity
                       deriving (Show, Read, Eq)

pquantShow :: ParticipationQuantity -> String
pquantShow (Limited x) = show x
pquantShow Unlimited = "*"

ptypeShow :: ParticipationType -> String
ptypeShow pt = case pt of
   MandatoryUnique -> "1"
   OptionalUnique -> "0..1"
   (Mandatory x) -> "1.." ++ pquantShow x
   (Optional x) -> "0.." ++ pquantShow x
   (Custom x y) -> pquantShow x ++ ".." ++ pquantShow y


data MetaAttribute = MetaAttribute { maName :: String, maType :: String, maValue :: String } deriving (Show,Read,Eq)
data MetaParticipation = MetaParticipation { mpName :: String, mpType :: String, mpIdentifier :: String, mpPType :: ParticipationType } deriving (Show,Read,Eq)

tupleToAttribute :: (String, String, String) -> MetaAttribute
tupleToAttribute (a, b, c) = MetaAttribute { maName = a, maType = b, maValue = c }

tupleToParticipation :: (String, String, String, ParticipationType) -> MetaParticipation
tupleToParticipation (a, b, c, t) = MetaParticipation { mpName = a, mpType = b, mpIdentifier = c, mpPType = t }

data MetaElement = MetaEntity { meName :: String, meIdentifier :: String, meAttributes :: [MetaAttribute], meValid :: Bool}
                 | MetaRelationship { mrName :: String, mrIdentifier :: String, mrParticipations :: [MetaParticipation], mrValid :: Bool}
                 | MetaModel { mmName :: Maybe String, mmIdentifier :: Maybe String, mmElements :: [MetaElement], mmValid :: Bool}
                 deriving (Show, Read, Eq)

instance CMElement MetaElement where
  toMeta = Just
  elementName MetaEntity { .. } = meName
  elementName MetaRelationship { .. } = mrName
  elementName MetaModel { .. } = fromMaybe "" mmName

instance Identifiable MetaElement where
  identifier MetaEntity { .. } = meIdentifier
  identifier MetaRelationship { .. } = mrIdentifier
  identifier MetaModel { .. } = fromMaybe "" mmIdentifier

instance Entity MetaElement where
  entityAttributes MetaEntity { .. } = map tupleToAttribute [("name", "String", meName), ("attributes", "[MetaAttribute]", show meAttributes)]
  entityAttributes MetaRelationship { .. } = map tupleToAttribute [("name", "String", mrName), ("participants", "[MetaParticipation]", show mrParticipations)]
  entityAttributes MetaModel { .. } = map tupleToAttribute [("name", "String", fromMaybe "" mmName), ("elements", "[MetaElement]", show mmElements)]
