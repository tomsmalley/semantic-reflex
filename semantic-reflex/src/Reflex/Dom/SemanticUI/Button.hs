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

import           Data.Default
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Reflex.Dom.Core hiding (fromJSString)
import           Reflex.Dom.SemanticUI.Common
import           Reflex.Dom.SemanticUI.Icon

data Button t = Button
  { _label :: Active t Text
  , _config :: ButtonConfig t
  }

data ButtonConfig t = ButtonConfig
  { _color :: Active t (Maybe Color)
  , _disabled :: Active t Bool
  , _icon :: RenderWhen t (Icon t)
  , _attached :: Active t (Maybe ExclusiveAttached)
  }

instance Default (ButtonConfig t) where
  def = ButtonConfig
    { _color = Static Nothing
    , _disabled = Static False
    , _icon = NeverRender
    , _attached = Static Nothing
    }

buttonConfigClasses :: Reflex t => ButtonConfig t -> Active t ClassText
buttonConfigClasses ButtonConfig {..} = mconcat
  [ memptyUnless "disabled" <$> _disabled
  , toClassText <$> _color
  , toClassText <$> _attached
  ]

instance t' ~ t => UI t' m (Button t) where
  type Return t' m (Button t) = Event t ()

  ui' (Button label config@ButtonConfig {..}) = do
    (e, _) <- elActiveAttr' "button" attrs $ do
      runRenderWhen ui' _icon
      activeText label
    return (e, domEvent Click e)
    where
      attrs = mkAttrs <$> buttonConfigClasses config
      mkAttrs c = "class" =: getClass (mconcat ["ui button", c])
