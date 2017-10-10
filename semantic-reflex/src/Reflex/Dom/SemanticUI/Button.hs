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

module Reflex.Dom.SemanticUI.Button where

import Data.Default
import Data.Maybe
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core hiding (fromJSString)

import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Icon
import Reflex.Dom.SemanticUI.Transition

data Button t = Button
  { _label :: Active t Text
  , _config :: ButtonConfig t
  }

data ButtonConfig t = ButtonConfig
  { _color :: Active t (Maybe Color)
  , _size :: Active t (Maybe Size)
  , _disabled :: Active t Bool
  , _compact :: Active t Bool
  , _icon :: RenderWhen t (Icon t)
  , _attached :: Active t (Maybe ExclusiveAttached)
  , _config :: ActiveElConfig t
  }

instance Default (ButtonConfig t) where
  def = ButtonConfig
    { _color = Static Nothing
    , _size = Static Nothing
    , _disabled = Static False
    , _compact = Static False
    , _icon = NeverRender
    , _attached = Static Nothing
    , _config = def
    }

buttonConfigClasses :: Reflex t => ButtonConfig t -> Active t Classes
buttonConfigClasses ButtonConfig {..} = activeClasses
  [ Static $ Just "ui button"
  , boolClass "disabled" _disabled
  , boolClass "compact" _compact
  , fmap toClassText <$> _color
  , fmap toClassText <$> _size
  , fmap toClassText <$> _attached
  ]

instance t' ~ t => UI t' m (Button t) where
  type Return t' m (Button t) = Event t ()

  ui' (Button label config@ButtonConfig {..}) = do
    (e, _) <- elWithAnim' "button" attrs $ do
      runRenderWhen ui' _icon
      activeText label
    return (e, domEvent Click e)
    where
      attrs = _config <> def
        { _classes = buttonConfigClasses config
        }
