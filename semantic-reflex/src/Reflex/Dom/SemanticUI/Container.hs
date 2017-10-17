{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE RecursiveDo              #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}

module Reflex.Dom.SemanticUI.Container where

import Data.Default
import Reflex.Dom.Core

import Reflex.Dom.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data Container t m a = Container
  { _config :: ContainerConfig t
  , _contents :: Component None m a
  }

data ContainerConfig t = ContainerConfig
  { _size  :: Active t (Maybe Size)
  , _config :: ActiveElConfig t
  }

instance Reflex t => Default (ContainerConfig t) where
  def = ContainerConfig
    { _size = pure Nothing
    , _config = def
    }

containerConfigClasses :: Reflex t => ContainerConfig t -> Active t Classes
containerConfigClasses ContainerConfig {..} = activeClasses
    [ Static $ Just $ "ui container"
    , fmap toClassText <$> _size
    ]

