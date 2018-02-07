{-# LANGUAGE TemplateHaskell #-}

module Reflex.Dom.SemanticUI.Divider
  (

    divider, divider'
  , contentDivider, contentDivider'
  , DividerConfig (..)
  , dividerInverted
  , dividerFitted
  , dividerHidden
  , dividerSection
  , dividerClearing
  , dividerElConfig

  ) where

import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
import Control.Monad (void)
import Data.Default
import Data.Semigroup ((<>))
import Reflex.Dom.Core

import Reflex.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data DividerConfig t = DividerConfig
  { _dividerInverted :: Active t Bool
  , _dividerFitted :: Active t Bool
  , _dividerHidden :: Active t Bool
  , _dividerSection :: Active t Bool
  , _dividerClearing :: Active t Bool
  , _dividerElConfig :: ActiveElConfig t
  }
makeLensesWith (lensRules & simpleLenses .~ True) 'DividerConfig

instance HasElConfig t (DividerConfig t) where
  elConfig = dividerElConfig

instance Reflex t => Default (DividerConfig t) where
  def = DividerConfig
    { _dividerInverted = pure False
    , _dividerFitted = pure False
    , _dividerHidden = pure False
    , _dividerSection = pure False
    , _dividerClearing = pure False
    , _dividerElConfig = def
    }

dividerConfigClasses :: Reflex t => DividerConfig t -> Active t Classes
dividerConfigClasses DividerConfig {..} = dynClasses
  [ pure $ Just "ui divider"
  , boolClass "inverted" _dividerInverted
  , boolClass "fitted" _dividerFitted
  , boolClass "hidden" _dividerHidden
  , boolClass "section" _dividerSection
  , boolClass "clearing" _dividerClearing
  ]

-- | In semantic-ui terms, this is a horizontal divider. Vertical dividers are
-- not implemented due to them being broken:
-- https://github.com/Semantic-Org/Semantic-UI/issues/4342
divider'
  :: UI t m => DividerConfig t
  -> m (Element EventResult (DomBuilderSpace m) t)
divider' config@DividerConfig {..}
  = fst <$> uiElement' "div" elConf blank
  where
    elConf = _dividerElConfig <> def
      { _classes = dividerConfigClasses config }

-- | In semantic-ui terms, this is a horizontal divider. Vertical dividers are
-- not implemented due to them being broken:
-- https://github.com/Semantic-Org/Semantic-UI/issues/4342
divider :: UI t m => DividerConfig t -> m ()
divider = void . divider'

contentDivider'
  :: UI t m => DividerConfig t -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
contentDivider' config@DividerConfig {..} content
  = uiElement' "div" elConf content
  where
    elConf = _dividerElConfig <> def
      { _classes = addClass "horizontal" <$> dividerConfigClasses config }

contentDivider :: UI t m => DividerConfig t -> m a -> m a
contentDivider c = fmap snd . contentDivider' c

