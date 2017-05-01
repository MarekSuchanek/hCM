module CM.Helpers where

data Validity = Valid | Invalid String

isValid :: Validity -> Bool
isValid Valid = True
isValid _     = False

violationMessage :: Validity -> Maybe String
violationMessage (Invalid msg) = Just msg
violationMessage _             = Nothing
