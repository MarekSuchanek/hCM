{-|
Module      : CM.Validity
Description : Tools for working with validity
Copyright   : (c) Marek Suchánek, 2017
License     : MIT

Constructions for working with conceptual model elements validity and
constraints.
-}
module CM.Validity where

-- |Type showing validity of some contruct (similar to 'Maybe')
data Validity = Valid | Invalid String deriving (Show, Read, Eq)

-- |Check if 'Validity' signals valid or invalid
isValid :: Validity -> Bool
isValid Valid = True
isValid _     = False

-- |Convert 'Validity' to 'Maybe' with message
violationMessage :: Validity -> Maybe String
violationMessage (Invalid msg) = Just msg
violationMessage _             = Nothing

-- |Construct constraint with boolean value and message
newConstraint :: Bool -> String -> Validity
newConstraint b msg = if b then Valid else Invalid msg
