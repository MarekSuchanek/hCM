{-|
Module      : CM.Helpers
Description : Helper constructs for conceptual modelling
Copyright   : (c) Marek Suchánek, 2017
License     : MIT

Helper constructs for conceptual modelling that are making
modelling with library easier or can be used often in different
way.
-}
module CM.Helpers where

import CM.Metamodel


-- |Convert 'ParticipationQuantity' to simple string representation
pquantShow :: ParticipationQuantity -> String
pquantShow (Limited x) = show x
pquantShow Unlimited = "*"
pquantShow Unique = "1"

-- |Convert 'ParticipationType' to simple string representation
ptypeShow :: ParticipationType -> String
ptypeShow pt = case pt of
              (Mandatory Unique) -> "1"
              (Optional Unique) -> "0..1"
              (Mandatory x) -> "1.." ++ pquantShow x
              (Optional x) -> "0.." ++ pquantShow x
              (Custom x y) -> pquantShow x ++ ".." ++ pquantShow y

-- |Find value of attribute with given name
findAttributeValue :: [MetaAttribute] -> String -> String
findAttributeValue [] _ = ""
findAttributeValue (x:xs) a = if a == maName x then maValue x
                                               else findAttributeValue xs a

-- |Find identifier of participant with given name
findParticipantId :: [MetaParticipation] -> String -> String
findParticipantId [] _ = ""
findParticipantId (x:xs) a = if a == mpName x then mpIdentifier x
                                              else findParticipantId xs a
