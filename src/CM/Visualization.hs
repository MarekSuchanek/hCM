{-# LANGUAGE RecordWildCards #-}
module CM.Visualization where

import Data.Maybe
import Data.List
import Data.Hashable
import CM.Metamodel

nl2br        :: String -> String
nl2br []     = ""
nl2br (x:xs) = if x == '\n' then "<BR/>" ++ nl2br xs else x : nl2br xs

metaElementToDotModel :: (ConceptualModel m) => m -> MetaElement -> String
metaElementToDotModel model MetaEntity { .. } = "\"" ++ meName ++ "\" [shape=none, margin=0, label=<\n" ++ table ++ "\n>];\n" ++ supers
  where table = start ++ header ++ rows ++ end
        start = "\t<table border=\"0\" cellborder=\"1\" cellspacing=\"0\" cellpadding=\"4\">\n"
        header = "\t\t<tr><td colspan=\"3\" bgcolor=\"lightblue\">" ++ meName ++ "</td></tr>\n"
        rows = concatMap metaAttributeToRow meAttributes
        end = "\t</table>"
        supers = intercalate "\n" . map superToModelLink $ meSuperNames
        superToModelLink x = meName ++ " -> " ++ x ++ " [arrowhead=empty];"
        metaAttributeToRow MetaAttribute { .. } = "\t\t<tr><td align=\"left\">" ++ maName ++ "</td><td>::</td><td align=\"left\">" ++ maType ++ "</td></tr>\n"
metaElementToDotModel model MetaRelationship { .. } = "\"" ++ mrName ++ "\" [shape=diamond];\n" ++ participationLinks
  where participationLinks = concatMap metaParticipationToModelLink mrParticipations
        metaParticipationToModelLink MetaParticipation { .. } = "\"" ++ mrName ++ "\" -> \"" ++ mpType ++ "\" [label=\""++ mpName ++" \\n ["++ ptypeShow mpPType ++"]\"];\n"
metaElementToDotModel model MetaModel { .. } = "digraph CM_model {\n" ++ graphset ++ nodeset ++ edgeset ++ content ++ "}\n"
  where graphset = "graph[rankdir=TD, overlap=false, splines=true, label=\"" ++ fromMaybe "" mmName ++ "\"];\n"
        nodeset  = "node [shape=record, fontsize=10, fontname=\"Verdana\"];\n"
        edgeset  = "edge [arrowhead=none, fontsize=10];\n\n"
        content  =  intercalate "\n" . map (elementToDotModel model) . filterEntitiesType $ mmElements

filterEntitiesType :: [MetaElement] -> [MetaElement]
filterEntitiesType = filterHelper []
  where filterHelper seen [] = seen
        filterHelper seen (x:xs)
          | xTypeSeen = filterHelper seen xs
          | otherwise = filterHelper (seen ++ [x]) xs
          where xTypeSeen = any (\e -> elementName x == elementName e) seen


elementToDotModel :: (ConceptualModel m, CMElement e) => m -> e -> String
elementToDotModel model element = metaElementToDotModel model (toMeta model element)

modelToDotModel :: (ConceptualModel m) => m -> String
modelToDotModel model = elementToDotModel model model

metaElementToDotInstance :: (ConceptualModel m) => m -> MetaElement -> String
metaElementToDotInstance model MetaEntity { .. } = "\"" ++ dotId ++ "\" [shape=none, margin=0, label=<\n" ++ table ++ "\n>];\n"
  where table = start ++ header ++ rows ++ end
        start = "\t<table border=\"0\" cellborder=\"1\" cellspacing=\"0\" cellpadding=\"4\">\n"
        header = "\t\t<tr><td colspan=\"3\" bgcolor=\"" ++ color ++ "\"><u>" ++ displayId ++ ":" ++ meName ++ "</u></td></tr>\n"
        rows = concatMap metaAttributeToRow meAttributes
        end = "\t</table>"
        color = if meValid then "lightgreen" else "darksalmon"
        dotId = makeDotId meIdentifier
        displayId = makeDisplayId meIdentifier
        metaAttributeToRow MetaAttribute { .. } = "\t\t<tr><td align=\"left\">" ++ maName ++ "</td><td>=</td><td align=\"left\">" ++ nl2br maValue ++ "</td></tr>\n"
metaElementToDotInstance model MetaRelationship { .. } = "\"" ++ dotId ++ "\" [shape=diamond, label=<<u>" ++ displayId ++ ":" ++ mrName ++ "</u>>, style=\"filled\", fillcolor=\"" ++ color ++ "\"];\n" ++ participationLinks
  where participationLinks = concatMap metaParticipationToLink mrParticipations
        metaParticipationToLink MetaParticipation { .. } = "\"" ++ dotId ++ "\" -> \"" ++ makeDotId mpIdentifier ++ "\" [label=\"" ++ mpName ++ "\"];\n"
        dotId = makeDotId mrIdentifier
        displayId = makeDisplayId mrIdentifier
        color = if mrValid then "lightgreen" else "darksalmon"
metaElementToDotInstance model MetaModel { .. } = "digraph CM_instance {\n" ++ graphset ++ nodeset ++ edgeset ++ content ++ "}\n"
  where graphset = "graph[rankdir=TD, overlap=false, splines=true, label=<<u>" ++ fromMaybe "" mmIdentifier ++ ":" ++ fromMaybe "" mmName ++ "</u>>];\n"
        nodeset  = "node [shape=record, fontsize=10, fontname=\"Verdana\"];\n"
        edgeset  = "edge [arrowhead=none, fontsize=10];\n\n"
        content  =  intercalate "\n" . map (metaElementToDotInstance model) . filterEntitiesInstance $ mmElements

makeDotId :: String -> String
makeDotId s = if length s > 10 then "h" ++ (show . hash $ s) else s

makeDisplayId :: String -> String
makeDisplayId s = if length s > 10 then "" else s

filterEntitiesInstance :: [MetaElement] -> [MetaElement]
filterEntitiesInstance = filterHelper []
  where filterHelper seen [] = seen
        filterHelper seen (x:xs)
          | xTypeSeen = filterHelper seen xs
          | otherwise = filterHelper (seen ++ [x]) xs
          where xTypeSeen = any (\e -> identifier x == identifier e) seen

elementToDotInstance :: (ConceptualModel m, CMElement e) => m -> e -> String
elementToDotInstance model element = metaElementToDotInstance model (toMeta model element)

modelToDotInstance :: (ConceptualModel m) => m -> String
modelToDotInstance model = elementToDotInstance model model
