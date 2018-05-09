-- | Semantic UI rails.
-- https://semantic-ui.com/elements/rail.html
module Reflex.Dom.SemanticUI.Rail
  (

  -- * Rail
    rail, rail'
  , RailConfig (..)
  , RailSide (..)
  , railConfig_dividing
  , railConfig_internal
  , railConfig_attached
  , railConfig_close
  , railConfig_size
  , railConfig_elConfig

  ) where

#ifdef USE_TEMPLATE_HASKELL
import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
#else
import Control.Lens.Type
#endif

import Data.Default
import Data.Semigroup ((<>))
import Reflex
import Reflex.Dom.Core

import Reflex.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data RailSide = LeftRail | RightRail
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToClassText RailSide where
  toClassText LeftRail = "left"
  toClassText RightRail = "right"

data RailClose = Close | VeryClose
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToClassText RailClose where
  toClassText Close = "close"
  toClassText VeryClose = "very close"

data RailConfig t = RailConfig
  { _railConfig_dividing :: Active t Bool
  , _railConfig_internal :: Active t Bool
  , _railConfig_attached :: Active t Bool
  , _railConfig_close :: Active t (Maybe RailClose)
  , _railConfig_size :: Active t (Maybe Size)
  , _railConfig_elConfig :: ActiveElConfig t
  }
#ifdef USE_TEMPLATE_HASKELL
makeLensesWith (lensRules & simpleLenses .~ True) ''RailConfig
#endif

instance HasElConfig t (RailConfig t) where
  elConfig = railConfig_elConfig

instance Reflex t => Default (RailConfig t) where
  def = RailConfig
    { _railConfig_dividing = pure False
    , _railConfig_internal = pure False
    , _railConfig_attached = pure False
    , _railConfig_close = pure Nothing
    , _railConfig_size = pure Nothing
    , _railConfig_elConfig = def
    }

-- | Make the rail div classes from the configuration
railConfigClasses :: Reflex t => RailConfig t -> Active t Classes
railConfigClasses RailConfig {..} = dynClasses
  [ pure $ Just "ui rail"
  , boolClass "dividing" _railConfig_dividing
  , boolClass "internal" _railConfig_internal
  , boolClass "attached" _railConfig_attached
  , fmap toClassText <$> _railConfig_close
  , fmap toClassText <$> _railConfig_size
  ]

-- | Rail UI Element.
rail'
  :: UI t m => RailSide -> RailConfig t -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
rail' railSide config@RailConfig {..} content
  = ui' "div" elConf content
  where
    elConf = _railConfig_elConfig <> def
      { _classes = addClass (toClassText railSide)
               <$> railConfigClasses config }

-- | Rail UI Element.
rail :: UI t m => RailSide -> RailConfig t -> m a -> m a
rail railSide config = fmap snd . rail' railSide config

#ifndef USE_TEMPLATE_HASKELL
#include "Rail.th.hs"
#endif
