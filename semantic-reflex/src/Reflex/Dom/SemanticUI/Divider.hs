module Reflex.Dom.SemanticUI.Divider
  (

    divider, divider'
  , contentDivider, contentDivider'
  , DividerConfig (..)
  , dividerConfig_inverted
  , dividerConfig_fitted
  , dividerConfig_hidden
  , dividerConfig_section
  , dividerConfig_clearing
  , dividerConfig_elConfig

  ) where

#ifdef USE_TEMPLATE_HASKELL
import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
#else
import Control.Lens.Type
#endif

import Control.Monad (void)
import Data.Default
import Data.Semigroup ((<>))
import Reflex.Dom.Core

import Reflex.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data DividerConfig t = DividerConfig
  { _dividerConfig_inverted :: Active t Bool
  , _dividerConfig_fitted :: Active t Bool
  , _dividerConfig_hidden :: Active t Bool
  , _dividerConfig_section :: Active t Bool
  , _dividerConfig_clearing :: Active t Bool
  , _dividerConfig_elConfig :: ActiveElConfig t
  }
#ifdef USE_TEMPLATE_HASKELL
makeLensesWith (lensRules & simpleLenses .~ True) 'DividerConfig
#endif

instance HasElConfig t (DividerConfig t) where
  elConfig = dividerConfig_elConfig

instance Reflex t => Default (DividerConfig t) where
  def = DividerConfig
    { _dividerConfig_inverted = pure False
    , _dividerConfig_fitted = pure False
    , _dividerConfig_hidden = pure False
    , _dividerConfig_section = pure False
    , _dividerConfig_clearing = pure False
    , _dividerConfig_elConfig = def
    }

dividerConfigClasses :: Reflex t => DividerConfig t -> Active t Classes
dividerConfigClasses DividerConfig {..} = dynClasses
  [ pure $ Just "ui divider"
  , boolClass "inverted" _dividerConfig_inverted
  , boolClass "fitted" _dividerConfig_fitted
  , boolClass "hidden" _dividerConfig_hidden
  , boolClass "section" _dividerConfig_section
  , boolClass "clearing" _dividerConfig_clearing
  ]

-- | In semantic-ui terms, this is a horizontal divider. Vertical dividers are
-- not implemented due to them being broken:
-- https://github.com/Semantic-Org/Semantic-UI/issues/4342
divider'
  :: UI t m => DividerConfig t
  -> m (Element EventResult (DomBuilderSpace m) t)
divider' config@DividerConfig {..}
  = fst <$> ui' "div" elConf blank
  where
    elConf = _dividerConfig_elConfig <> def
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
  = ui' "div" elConf content
  where
    elConf = _dividerConfig_elConfig <> def
      { _classes = addClass "horizontal" <$> dividerConfigClasses config }

contentDivider :: UI t m => DividerConfig t -> m a -> m a
contentDivider c = fmap snd . contentDivider' c

#ifndef USE_TEMPLATE_HASKELL
#include "Divider.th.hs"
#endif
