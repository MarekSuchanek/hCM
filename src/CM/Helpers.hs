module CM.Helpers where

data Validity = Valid | Invalid String deriving (Show, Read, Eq)

isValid :: Validity -> Bool
isValid Valid = True
isValid _     = False

violationMessage :: Validity -> Maybe String
violationMessage (Invalid msg) = Just msg
violationMessage _             = Nothing

newConstraint :: Bool -> String -> Validity
newConstraint b msg = if b then Valid else Invalid msg
