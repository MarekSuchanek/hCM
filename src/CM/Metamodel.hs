{-# LANGUAGE RecordWildCards #-}
{-|
Module      : CM.Metamodel
Description : Conceptual model metamodel
Copyright   : (c) Marek Suchánek, 2017
License     : MIT

Metamodel used to create actual models within system provides simple
way for compiler-driven modelling without restricting usage of various
Haskell constructs of structure and behavior coding.
-}
module CM.Metamodel where

import Data.Maybe
import CM.Validity

-- |Type synonym for identifier use
type Identifier = String

-- |Class encapsulating the identity principle
class (Show i) => Identifiable i where
  -- |Get identifier of element (default: show)
  identifier :: i -> Identifier
  identifier = show
  -- |Check if two elements are identic
  identic :: i -> i -> Bool
  identic x y = identifier x == identifier y

-- |Generic element of conceptual model
class (Show e, Read e) => CMElement e where
  -- |List of simple contraints (no need to know its model)
  simpleConstraints :: [e -> Validity]
  simpleConstraints = []
  -- |List of complex (model-aware) contraints
  complexConstaints :: (ConceptualModel m) => [m -> e -> Validity]
  complexConstaints = []
  -- |List of all contraints (default: simple + complex)
  constraints :: (ConceptualModel m) => m -> [e -> Validity]
  constraints model = simpleConstraints ++ map (\f -> f model) complexConstaints
  -- |Evaluate all contraints and get list of 'Validity' as result
  evalConstraints :: (ConceptualModel m) => m -> e -> [Validity]
  evalConstraints model x = map ($ x) $ constraints model
  -- |Check if element is valid (conforms all constraints)
  valid :: (ConceptualModel m) => m -> e -> Bool
  valid model x = all isValid (evalConstraints model x)
  -- |Get list of all constraints violations (empty list if valid)
  violations :: (ConceptualModel m) => m -> e -> [String]
  violations model x = mapMaybe violationMessage $ evalConstraints model x
  -- |Name of element type (default is derived by 'show')
  elementName :: e -> String
  elementName = takeWhile (/= ' ') . show
  -- |Convert element to 'MetaElement' (recommended: use functions from subclasses)
  toMeta :: (ConceptualModel m) => m -> e -> MetaElement
  -- |Optional function from converting back from 'MetaElement'
  fromMeta :: e -> MetaElement -> Maybe e
  fromMeta _ _ = Nothing

-- |Whole conceptual model class
class (CMElement a) => ConceptualModel a where
  -- |Get all elements of the model
  cmodelElements :: a -> [MetaElement]
  -- |Check whether all elements in model are valid and is valid itself
  validModel :: a -> Bool
  validModel m = validSelf && validElements
                where validSelf = valid m m
                      validElements = all metaElementValid (cmodelElements m)
  -- |Name of model type (default: 'elementName')
  cmodelName :: a -> String
  cmodelName = elementName
  -- |Convert to 'MetaModel' with use of 'cmodelName' and 'cmodelElements'
  toMetaModel :: (ConceptualModel b) => b -> a -> MetaElement
  toMetaModel _ m = MetaModel { mmName = Just $ cmodelName m
                              , mmElements = cmodelElements m
                              , mmIdentifier = Nothing
                              , mmValid = validModel m
                              }

-- |Entities for representing concepts
class (CMElement a, Identifiable a) => Entity a where
  -- |Get all attributes of the entity
  entityAttributes :: a -> [MetaAttribute]
  -- |Name of entity type (default: 'elementName')
  entityName :: a -> String
  entityName = elementName
  -- |Name of supertypes (default: '[]')
  entitySuperNames :: a -> [String]
  entitySuperNames _ = []
  -- |Names of subtypes (default: '[]')
  entitySubNames :: a -> [String]
  entitySubNames _ = []
  -- |Convert to 'MetaEntity' with use of 'entityName', 'entityAttributes' and others
  toMetaEntity :: (ConceptualModel m) => m -> a -> MetaElement
  toMetaEntity m x = MetaEntity { meName = entityName x
                                , meAttributes = entityAttributes x
                                , meIdentifier = identifier x
                                , meValid = valid m x
                                , meSuperNames = entitySuperNames x
                                , meSubNames = entitySubNames x
                                }

-- |Relationship that connects multiple entities
class (CMElement a, Identifiable a) => Relationship a where
  -- |Get all participations in the relationship
  relationshipParticipations :: a -> [MetaParticipation]
  -- |Name of relationship type (default: 'elementName')
  relationshipName :: a -> String
  relationshipName = elementName
  -- |Convert to 'MetaRelationship' with use of 'relationshipName' and 'relationshipParticipations'
  toMetaRelationship :: (ConceptualModel m) => m -> a -> MetaElement
  toMetaRelationship m x = MetaRelationship { mrName = relationshipName x
                                            , mrParticipations = relationshipParticipations x
                                            , mrIdentifier = identifier x
                                            , mrValid = valid m x
                                            }
--------------------------------------------------------------------------------
-- |Quantity of participation type
data ParticipationQuantity
  = Limited Word -- ^ Quantity limited by positive number
  | Unlimited    -- ^ Unlimited quantity (i.e. infinity)
  | Unique       -- ^ Special unique (i.e. always must be exactly 1)
  deriving (Show, Read, Eq, Ord)

-- |Type of participation (i.e. multiplicity)
data ParticipationType
  = Mandatory ParticipationQuantity                    -- ^ Type for mandatory relationship (1..N)
  | Optional ParticipationQuantity                     -- ^ Type for optional relationship (0..N)
  | Custom ParticipationQuantity ParticipationQuantity -- ^ Type for custom relationship (X..Y)
  deriving (Show, Read, Eq)

-- |Attribute representation in meta level
data MetaAttribute = MetaAttribute { maName :: String
                                   , maType :: String
                                   , maValue :: String
                                   } deriving (Show, Read, Eq)

-- |Participation representation in meta level
data MetaParticipation = MetaParticipation { mpName       :: String
                                           , mpType       :: String
                                           , mpIdentifier :: String
                                           , mpPType      :: ParticipationType
                                           } deriving (Show, Read, Eq)

-- |'CMElement' representation in meta level
data MetaElement
  -- |'Entity' representation in meta level
  = MetaEntity { meName       :: String
               , meIdentifier :: String
               , meAttributes :: [MetaAttribute]
               , meValid      :: Bool
               , meSuperNames :: [String]
               , meSubNames   :: [String]
               }
  -- |'Relationship' representation in meta level
  | MetaRelationship { mrName           :: String
                     , mrIdentifier     :: String
                     , mrParticipations :: [MetaParticipation]
                     , mrValid          :: Bool
                     }
  -- |'ConceptualModel' representation in meta level
  | MetaModel { mmName       :: Maybe String
              , mmIdentifier :: Maybe String
              , mmElements   :: [MetaElement]
              , mmValid      :: Bool
              }
  deriving (Show, Read, Eq)

-- |Get element name from any 'MetaElement'
metaElementName :: MetaElement -> String
metaElementName MetaEntity {..} = meName
metaElementName MetaRelationship {..} = mrName
metaElementName MetaModel {..} = fromMaybe "" mmName

-- |Get identifier from any 'MetaElement'
metaElementIdentifier :: MetaElement -> String
metaElementIdentifier MetaEntity {..} = meIdentifier
metaElementIdentifier MetaRelationship {..} = mrIdentifier
metaElementIdentifier MetaModel {..} = fromMaybe "" mmIdentifier

-- |Check if any 'MetaElement' is valid
metaElementValid :: MetaElement -> Bool
metaElementValid MetaEntity {..} = meValid
metaElementValid MetaRelationship {..} = mrValid
metaElementValid MetaModel {..} = mmValid

-- |Convert tuple to 'MetaAttribute'
tupleToAttribute :: (String, String, String) -> MetaAttribute
tupleToAttribute (a, b, c) = MetaAttribute {maName = a, maType = b, maValue = c}

-- |Convert tuple to 'MetaParticipation'
tupleToParticipation :: (String, String, String, ParticipationType) -> MetaParticipation
tupleToParticipation (a, b, c, t) = MetaParticipation {mpName = a, mpType = b, mpIdentifier = c, mpPType = t}

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
