{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Reflex.Dom.SemanticUI.TH where

import Control.Monad (replicateM)
import Data.Char (toLower)
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Class

-- | Make top level function declarations for the given semantic component type.
-- Given a data type like:
--
--     data Element
--      = MyElement Text ElementConfig
--      | MyOtherElement Text Int OtherElementConfig
--
-- The template haskell:
--
--     $(makeFunctions ''Element)
--
-- will generate top level definitions equivalent to:
--
--     myElement a b = ui (MyElement a b)
--     myElement' a b = ui' (MyElement a b)
--     myElement_ a b = ui_ (MyElement a b)
--
--     myOtherElement a b c = ui (MyOtherElement a b c)
--     myOtherElement' a b c = ui' (MyOtherElement a b c)
--     myOtherElement_ a b c = ui_ (MyOtherElement a b c)
--
makeFunctions :: Name -> DecsQ
makeFunctions name = do
  typeInfo <- reifyDatatype name

  -- Make declarations for each data constructor
  fmap concat $ traverse makeDecs (datatypeCons typeInfo)

  where

    makeDecs :: ConstructorInfo -> Q [Dec]
    makeDecs info = do

      -- Function name is the constructor name with first character lowercase
      let funcNameBase = let (n : ns) = nameBase $ constructorName info
                          in (toLower n : ns)

      -- Generate a name for each required field
      fieldNames <- replicateM (length $ constructorFields info) (newName "x")

      -- Makes the function body, applies the constructor name to the fields
      let exp [field] = appE (conE $ constructorName info) (varE field)
          exp (field : fields) = appE (exp fields) (varE field)

      -- Make the top level function declaration using 'postfix' to modify the
      -- function name. We only have a single clause of all fieldNames and a
      -- body which applies 'func' to the output of 'exp'. Pattern names and
      -- expression names must be in opposite orders or the function will not be
      -- typed the same as the data constructor.
      let run postfix func = funD (mkName $ funcNameBase ++ postfix)
            [clause (varP <$> fieldNames)
                    (normalB $ appE (varE func) (exp $ reverse fieldNames))
              [] ]

      -- Generate all variants
      sequence [run "" 'ui, run "'" 'ui', run "_" 'ui_]

