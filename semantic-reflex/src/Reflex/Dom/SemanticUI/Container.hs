{-# LANGUAGE TemplateHaskell #-}

module Reflex.Dom.SemanticUI.Container
  (

  -- * Containers
    container, container'
  , ContainerConfig (..)
  , containerSize
  , containerElConfig

  ) where

import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
import Data.Default
import Data.Semigroup ((<>))
import Reflex.Dom.Core

import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data ContainerConfig t = ContainerConfig
  { _containerSize      :: Dynamic t (Maybe Size)
  , _containerElConfig  :: ActiveElConfig t
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''ContainerConfig

instance HasElConfig t (ContainerConfig t) where
  elConfig = containerElConfig

instance Reflex t => Default (ContainerConfig t) where
  def = ContainerConfig
    { _containerSize = pure Nothing
    , _containerElConfig = def
    }

containerConfigClasses :: Reflex t => ContainerConfig t -> Dynamic t Classes
containerConfigClasses ContainerConfig {..} = dynClasses
    [ pure $ Just $ "ui container"
    , fmap toClassText <$> _containerSize
    ]

container' :: MonadWidget t m => ContainerConfig t -> m a -> m (El t, a)
container' config@ContainerConfig {..} = uiElement' "div" elConf
    where
      elConf = _containerElConfig <> def
        { _classes = containerConfigClasses config }

container :: MonadWidget t m => ContainerConfig t -> m a -> m a
container config = fmap snd . container' config

