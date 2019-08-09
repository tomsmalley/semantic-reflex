{-# LANGUAGE TemplateHaskell #-}

-- | Semantic UI fields.
-- https://semantic-ui.com/collections/form.html
module Reflex.Dom.SemanticUI.Field
  (

  -- * Field
    field, field'
  , FieldConfig (..)
  , fieldConfig_error
  , fieldConfig_elConfig

  ) where

import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)

import Data.Default
import Data.Semigroup ((<>))
import Reflex
import Reflex.Dom.Core

import Reflex.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data FieldConfig t = FieldConfig
  { _fieldConfig_error :: Active t Bool
  , _fieldConfig_elConfig :: ActiveElConfig t
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''FieldConfig

instance HasElConfig t (FieldConfig t) where
  elConfig = fieldConfig_elConfig

instance Reflex t => Default (FieldConfig t) where
  def = FieldConfig
    { _fieldConfig_error = pure False
    , _fieldConfig_elConfig = def
    }

-- | Make the field div classes from the configuration
fieldConfigClasses :: Reflex t => FieldConfig t -> Active t Classes
fieldConfigClasses FieldConfig {..} = dynClasses
  [ pure $ Just "ui field"
  , boolClass "error" _fieldConfig_error
  ]

-- | Field UI Element.
field'
  :: UI js t m => FieldConfig t -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
field' config@FieldConfig {..} content
  = ui' "div" elConf content
  where
    elConf = _fieldConfig_elConfig <> def
      { _classes = fieldConfigClasses config }

-- | Field UI Element.
field :: UI js t m => FieldConfig t -> m a -> m a
field config = fmap snd . field' config
