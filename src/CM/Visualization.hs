module CM.Visualization
    (
      modelToDot,
      instanceToDot
    ) where

import Language.Haskell.Parser


-- TODO: process AST to own structures and then to DOT (Graphviz)
modelToDot :: String -> String
modelToDot = show . parseModule

-- TODO: implement similarly to modelToDot (with default Show?)
instanceToDot :: String -> String
instanceToDot x = "Not implemented"
