{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DuplicateRecordFields    #-}
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
{-# LANGUAGE GeneralizedNewtypeDeriving     #-}
{-# LANGUAGE GADTs     #-}

module Reflex.Dom.SemanticUI.Button where

import Control.Monad ((<=<))
import Control.Monad.Fix
import Data.Default
import Data.Maybe
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core hiding (fromJSString)

import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Icon
import Reflex.Dom.SemanticUI.Transition

data Conditional t = Conditional (ConditionalConfig t)
data ConditionalConfig t = ConditionalConfig
  { _dataText :: Active t (Maybe Text)
  }
instance Default (ConditionalConfig t) where
  def = ConditionalConfig
    { _dataText = Static Nothing
    }

data Buttons t m a = Buttons (ButtonsConfig t) (Restrict Buttons m a)

data ButtonsConfig t = ButtonsConfig
  { _color :: Active t (Maybe Color)
  , _size :: Active t (Maybe Size)
  , _basic :: Active t Bool
  , _attached :: Active t (Maybe VerticalAttached)
  , _width :: Active t (Maybe Width)
  , _config :: ActiveElConfig t
  }

instance Default (ButtonsConfig t) where
  def = ButtonsConfig
    { _color = Static Nothing
    , _size = Static Nothing
    , _basic = Static False
    , _attached = Static Nothing
    , _width = Static Nothing
    , _config = def
    }

buttonsConfigClasses :: Reflex t => ButtonsConfig t -> Active t Classes
buttonsConfigClasses ButtonsConfig {..} = activeClasses
  [ Static $ Just "ui buttons"
  , boolClass "basic" _basic
  , fmap toClassText <$> _color
  , fmap toClassText <$> _size
  , fmap toClassText <$> _attached
  , fmap toClassText <$> _width
  ]

data Button t = Button
  { _label :: Active t Text
  , _config :: ButtonConfig t
  }

data ButtonConfig t = ButtonConfig
  { _color :: Active t (Maybe Color)
  , _size :: Active t (Maybe Size)
  , _disabled :: Active t Bool
  , _compact :: Active t Bool
  , _basic :: Active t Bool
--  , _icon :: RenderWhen t (Icon t)
  , _attached :: Active t (Maybe ExclusiveAttached)
  , _realButton :: Bool
  , _config :: ActiveElConfig t
  }

instance Default (ButtonConfig t) where
  def = ButtonConfig
    { _color = Static Nothing
    , _size = Static Nothing
    , _disabled = Static False
    , _compact = Static False
    , _basic = Static False
--    , _icon = NeverRender
    , _attached = Static Nothing
    , _realButton = True
    , _config = def
    }

buttonConfigClasses :: Reflex t => ButtonConfig t -> Active t Classes
buttonConfigClasses ButtonConfig {..} = activeClasses
  [ Static $ Just "ui button"
  , boolClass "disabled" _disabled
  , boolClass "compact" _compact
  , boolClass "basic" _basic
  , fmap toClassText <$> _color
  , fmap toClassText <$> _size
  , fmap toClassText <$> _attached
  ]
