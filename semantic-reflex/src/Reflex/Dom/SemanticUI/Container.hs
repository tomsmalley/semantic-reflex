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
import Data.Maybe
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core hiding (fromJSString)

import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data Container t m a = Container
  { _config :: ContainerConfig t
  , _contents :: m a
  }

data ContainerConfig t = ContainerConfig
  { _size  :: Active t (Maybe Size)
  , _config :: ActiveElConfig t
  }

instance Default (ContainerConfig t) where
  def = ContainerConfig
    { _size = Static Nothing
    , _config = def
    }

containerConfigClasses :: Reflex t => ContainerConfig t -> Active t Classes
containerConfigClasses ContainerConfig {..} = activeClasses
    [ Static $ Just $ "ui container"
    , fmap toClassText <$> _size
    ]

instance (t ~ t', m ~ m') => UI t' m' (Container t m a) where
  type Return t' m' (Container t m a) = a
  ui' (Container config@ContainerConfig {..} contents)
    = elWithAnim' "i" attrs contents
    where
      attrs = _config <> def
        { _classes = containerConfigClasses config
        }

