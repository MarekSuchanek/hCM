{-# LANGUAGE RecordWildCards #-}

module CM.Metamodel where

import Data.Maybe
import CM.Helpers

type Identifier = String


class (Show i) => Identifiable i where
  identifier :: i -> Identifier
  identifier = show
  identic :: i -> i -> Bool
  identic x y = identifier x == identifier y

-- Generic element of conceptual model
class (Show e, Read e) => CMElement e where
  simpleConstraints :: [e -> Validity]
  simpleConstraints = []
  complexConstaints :: (ConceptualModel m) => [m -> e -> Validity]
  complexConstaints = []
  constraints :: (ConceptualModel m) => m -> [e -> Validity]
  constraints model = simpleConstraints ++ map (\f -> f model) complexConstaints
  evalConstraints :: (ConceptualModel m) => m -> e -> [Validity]
  evalConstraints model elem = map ($ elem) $ constraints model
  valid :: (ConceptualModel m) => m -> e -> Bool
  valid model elem = all isValid (evalConstraints model elem)
  violations :: (ConceptualModel m) => m -> e -> [String]
  violations model elem = mapMaybe violationMessage $ evalConstraints model elem
  elementName :: e -> String
  elementName = takeWhile (/= ' ') . show
  toMeta :: (ConceptualModel m) => m -> e -> MetaElement
  fromMeta :: e -> MetaElement -> Maybe e
  fromMeta _ _ = Nothing

-- Whole conceptual model class
class (CMElement a) => ConceptualModel a where
  cmodelElements :: a -> [MetaElement]
  cmodelName :: a -> String
  cmodelName = elementName
  toMetaModel :: (ConceptualModel b) => b -> a -> MetaElement
  toMetaModel x y = MetaModel { mmName = Just $ cmodelName y
                              , mmElements = cmodelElements y
                              , mmIdentifier = Nothing
                              , mmValid = valid x y -- TODO: valid elems + valid model
                              }

-- Entities for representing concepts
class (CMElement a, Identifiable a) => Entity a where
  entityAttributes :: a -> [MetaAttribute]
  entityName :: a -> String
  entityName = elementName
  entitySuperNames :: a -> [String]
  entitySuperNames _ = []
  toMetaEntity :: (ConceptualModel m) => m -> a -> MetaElement
  toMetaEntity m x = MetaEntity { meName = entityName x
                                , meAttributes = entityAttributes x
                                , meIdentifier = identifier x
                                , meValid = valid m x
                                , meSuperNames = entitySuperNames x
                                }

-- Relationship that connects multiple entities
class (CMElement a, Identifiable a) => Relationship a where
  relationshipParticipations :: a -> [MetaParticipation]
  relationshipName :: a -> String
  relationshipName = elementName
  relationshipSuperNames :: a -> [String]
  relationshipSuperNames _ = []
  toMetaRelationship :: (ConceptualModel m) => m -> a -> MetaElement
  toMetaRelationship m x = MetaRelationship { mrName = relationshipName x
                                            , mrParticipations = relationshipParticipations x
                                            , mrIdentifier = identifier x
                                            , mrValid = valid m x
                                            }

--------------------------------------------------------------------------------
data ParticipationQuantity
  = Limited Word
  | Unlimited
  | Unique
  deriving (Show, Read, Eq, Ord)

data ParticipationType
  = Mandatory ParticipationQuantity
  | Optional ParticipationQuantity
  | Custom ParticipationQuantity ParticipationQuantity
  deriving (Show, Read, Eq)

pquantShow :: ParticipationQuantity -> String
pquantShow (Limited x) = show x
pquantShow Unlimited = "*"

ptypeShow :: ParticipationType -> String
ptypeShow pt = case pt of
                (Mandatory Unique) -> "1"
                (Optional Unique) -> "0..1"
                (Mandatory x) -> "1.." ++ pquantShow x
                (Optional x) -> "0.." ++ pquantShow x
                (Custom x y) -> pquantShow x ++ ".." ++ pquantShow y

data MetaAttribute = MetaAttribute { maName :: String
                                   , maType :: String
                                   , maValue :: String
                                   } deriving (Show, Read, Eq)

data MetaParticipation = MetaParticipation { mpName       :: String
                                           , mpType       :: String
                                           , mpIdentifier :: String
                                           , mpPType      :: ParticipationType
                                           } deriving (Show, Read, Eq)

tupleToAttribute :: (String, String, String) -> MetaAttribute
tupleToAttribute (a, b, c) = MetaAttribute {maName = a, maType = b, maValue = c}

tupleToParticipation :: (String, String, String, ParticipationType) -> MetaParticipation
tupleToParticipation (a, b, c, t) = MetaParticipation {mpName = a, mpType = b, mpIdentifier = c, mpPType = t}

data MetaElement
  = MetaEntity { meName       :: String
               , meIdentifier :: String
               , meAttributes :: [MetaAttribute]
               , meValid      :: Bool
               , meSuperNames :: [String]
               }
  | MetaRelationship { mrName           :: String
                     , mrIdentifier     :: String
                     , mrParticipations :: [MetaParticipation]
                     , mrValid          :: Bool
                     }
  | MetaModel { mmName       :: Maybe String
              , mmIdentifier :: Maybe String
              , mmElements   :: [MetaElement]
              , mmValid      :: Bool
              }
  deriving (Show, Read, Eq)

instance CMElement MetaElement where
  toMeta _ = id
  elementName MetaEntity {..} = meName
  elementName MetaRelationship {..} = mrName
  elementName MetaModel {..} = fromMaybe "" mmName

instance Identifiable MetaElement where
  identifier MetaEntity {..} = meIdentifier
  identifier MetaRelationship {..} = mrIdentifier
  identifier MetaModel {..} = fromMaybe "" mmIdentifier

instance Entity MetaElement where
  entityAttributes MetaEntity {..} =
    map tupleToAttribute
      [ ("name", "String", meName)
      , ("attributes", "[MetaAttribute]", show meAttributes)
      ]
  entityAttributes MetaRelationship {..} =
    map tupleToAttribute
      [ ("name", "String", mrName)
      , ("participants", "[MetaParticipation]", show mrParticipations)
      ]
  entityAttributes MetaModel {..} =
    map tupleToAttribute
      [ ("name", "String", fromMaybe "" mmName)
      , ("elements", "[MetaElement]", show mmElements)
      ]

findAttributeValue :: [MetaAttribute] -> String -> String
findAttributeValue [] _ = ""
findAttributeValue (x:xs) a = if a == maName x then maValue x
                                             else findAttributeValue xs a

findParticipantId :: [MetaParticipation] -> String -> String
findParticipantId [] _ = ""
findParticipantId (x:xs) a = if a == mpName x then mpIdentifier x
                                            else findParticipantId xs a
