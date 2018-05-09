module Reflex.Dom.SemanticUI.Container
  (

  -- * Containers
    container, container'
  , ContainerConfig (..)
  , containerConfig_size
  , containerConfig_elConfig

  ) where

#ifdef USE_TEMPLATE_HASKELL
import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
#else
import Control.Lens.Type
#endif

import Data.Default
import Data.Semigroup ((<>))
import Reflex.Dom.Core

import Reflex.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data ContainerConfig t = ContainerConfig
  { _containerConfig_size      :: Active t (Maybe Size)
  , _containerConfig_elConfig  :: ActiveElConfig t
  }
#ifdef USE_TEMPLATE_HASKELL
makeLensesWith (lensRules & simpleLenses .~ True) ''ContainerConfig
#endif

instance HasElConfig t (ContainerConfig t) where
  elConfig = containerConfig_elConfig

instance Reflex t => Default (ContainerConfig t) where
  def = ContainerConfig
    { _containerConfig_size = pure Nothing
    , _containerConfig_elConfig = def
    }

containerConfigClasses
  :: Reflex t => ContainerConfig t -> Active t Classes
containerConfigClasses ContainerConfig {..} = dynClasses
    [ pure $ Just $ "ui container"
    , fmap toClassText <$> _containerConfig_size
    ]

container'
  :: UI t m => ContainerConfig t -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
container' config@ContainerConfig {..} = ui' "div" elConf
    where
      elConf = _containerConfig_elConfig <> def
        { _classes = containerConfigClasses config }

container :: UI t m => ContainerConfig t -> m a -> m a
container config = fmap snd . container' config

#ifndef USE_TEMPLATE_HASKELL
#include "Container.th.hs"
#endif
