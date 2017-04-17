
module CM.Visualization
    (
      modelToDot,
      instanceToDot
    ) where

import Data.Maybe
import Language.Haskell.Parser
import Language.Haskell.Syntax

data Entity = Entity {
  entityName :: String,
  entityAttributes :: [(String, String)]
} deriving Show

data EntityType = EntityType {
  entityTypeName :: String,
  entityTypeCons :: [Entity]
} deriving Show

data Model = Model {
  modelName :: String,
  modelTypes :: [EntityType]
} deriving Show

hsConDeclToEntity :: HsConDecl -> Entity
hsConDeclToEntity (HsConDecl _ (HsIdent name) types) = Entity {
  entityName = name,
  entityAttributes = map (\x -> ("", show x)) types
}
hsConDeclToEntity (HsRecDecl _ (HsIdent name) attrs) = Entity {
  entityName = name,
  entityAttributes = map parseAttribute attrs
}
    where parseAttribute (attrNames, attrType) = (parseName . head $ attrNames, parseType attrType)
          parseName (HsIdent str) = str
          parseType (HsBangedTy (HsTyCon (UnQual (HsIdent str)))) = str
          parseType (HsUnBangedTy (HsTyCon (UnQual (HsIdent str)))) = str

hsDeclToEntityType :: HsDecl -> Maybe EntityType
hsDeclToEntityType (HsDataDecl _ _ (HsIdent name) _ cons _) = Just EntityType {
  entityTypeName = name,
  entityTypeCons = map hsConDeclToEntity cons
}
hsDeclToEntityType _ = Nothing

hsModuleToModel :: HsModule -> Model
hsModuleToModel (HsModule _ (Module moduleName) _ _ decls) = Model {
  modelName = moduleName,
  modelTypes = catMaybes . map hsDeclToEntityType $ decls
}

-- TODO: process AST to own structures and then to DOT (Graphviz)
modelToDot :: String -> String
modelToDot input = case parseModule input of
                ParseOk m -> show . hsModuleToModel $ m
                ParseFailed srcLoc msg -> error ("Parse failed " ++ show srcLoc ++ ": " ++ msg)

-- TODO: implement similarly to modelToDot (with default Show?)
instanceToDot :: String -> String
instanceToDot x = "Not implemented"
