{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE CPP                      #-}
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
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core hiding (fromJSString)
import Reflex.Dom.SemanticUI.Common


data Container t m a = Container
  { _config :: ContainerConfig t
  , _contents :: m a
  }

data ContainerConfig t = ContainerConfig
  { _size  :: Active t (Maybe Size)
  }

instance Default (ContainerConfig t) where
  def = ContainerConfig
    { _size = Static Nothing
    }

containerConfigClasses :: Reflex t => ContainerConfig t -> Active t ClassText
containerConfigClasses ContainerConfig {..} = mconcat
    [ toClassText <$> _size
    ]

instance (t ~ t', m ~ m') => UI t' m' (Container t m a) where
  type Return t' m' (Container t m a) = a
  ui' Container {..} = elActiveAttr' "i" attrs _contents
    where
      attrs = mkAttrs <$> containerConfigClasses _config
      mkAttrs c = "class" =: getClass (mconcat ["ui container", c])

