module CM.Metamodel where

-- Generic element of conceptual model
class CMElement a where
  constraints :: [a -> Bool]
  constraints = []
  valid :: a -> Bool
  valid elem = all ($ elem) constraints

-- Entities for representing concepts
class (CMElement a) => Entity a

-- Relationship that connects multiple entities
class (CMElement a) => Relationship a

-- Participation of entity in relationship
class (CMElement a) => Participation a
