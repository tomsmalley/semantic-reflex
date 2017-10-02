{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}

module Reflex.Dom.SemanticUI.Checkbox
  (
  -- * Checkbox
    Checkbox (..)
  , CheckboxType (..)
  , Enabled (..)
  , Indeterminate (..)
  , Checked (..)
  -- * Return type
  , CheckboxResult (..)
  -- * Config
  , CheckboxConfig (..)

  ) where

import           Control.Monad (void)
import           Control.Monad.Trans (liftIO)
import           Control.Lens ((^.))
import           Data.Default (Default (def))
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified GHCJS.DOM.Element as DOM
import           Language.Javascript.JSaddle
import           Reflex
import           Reflex.Dom.Core hiding
  ( checkbox, Checkbox (..), checkbox_value, checkbox_change
  , CheckboxConfig (..), checkboxConfig_attributes, checkboxConfig_setValue
  )

import Reflex.Dom.SemanticUI.Common

--------------------------------------------------------------------------------
-- Types

-- | Checked state of a checkbox
data Checked = Checked | Unchecked deriving (Eq, Show)

-- | Indeterminate state of a checkbox
data Indeterminate = Determinate | Indeterminate deriving (Eq, Show)

-- | Enabled state of a checkbox
data Enabled = Enabled | Disabled deriving (Eq, Show)

data CheckboxResult t = CheckboxResult
  { _value :: Dynamic t Checked
  , _change :: Event t Checked
  , _enabled :: Dynamic t Enabled
  , _indeterminate :: Dynamic t Indeterminate
  }

instance HasValue (CheckboxResult t) where
  type Value (CheckboxResult t) = Dynamic t Checked
  value = _value

-- | Checkbox types according to https://semantic-ui.com/modules/checkbox.html.
-- If you need a radio type, see <RadioGroup>.
data CheckboxType =  Slider | Toggle | Fitted deriving (Eq, Show)

-- | Convert an option to its class representation
instance ToClassText CheckboxType where
  toClassText Slider = "slider"
  toClassText Toggle = "toggle"
  toClassText Fitted = "fitted"

data CheckboxConfig t = CheckboxConfig
  { _initialValue :: Checked
  -- ^ Initial value of checkbox (default: Unchecked)
  , _setValue :: Event t Checked
  -- ^ Event which sets the value
  , _initialEnabled :: Enabled
  -- ^ Initial enabled state of checkbox (default: Enabled)
  , _setEnabled :: Event t Enabled
  -- ^ Event which sets the enabled state
  , _initialIndeterminate :: Indeterminate
  -- ^ Initial indeterminate state of checkbox (default: Determinate)
  , _setIndeterminate :: Event t Indeterminate
  -- ^ Event which sets the indeterminate state
  , _attributes :: Map Text Text
  -- ^ Attributes of the \<input\> element
  , _divAttributes :: Map Text Text
  -- ^ Attributes of the wrapping \<div\> element
  , _types :: [CheckboxType]
  -- ^ Checkbox type (e.g. slider)
  }

instance Reflex t => Default (CheckboxConfig t) where
  def = CheckboxConfig
    { _initialValue = Unchecked
    , _setValue = never
    , _initialEnabled = Enabled
    , _setEnabled = never
    , _initialIndeterminate = Determinate
    , _setIndeterminate = never
    , _attributes = mempty
    , _divAttributes = mempty
    , _types = mempty
    }

data Checkbox t = Checkbox
  { _label :: Text
  , _config :: CheckboxConfig t
  }

instance UI t m (Checkbox t) where
  type Return t m (Checkbox t) = CheckboxResult t
  ui' (Checkbox label config) = checkbox' (text label) config

--------------------------------------------------------------------------------
-- Checkbox Functions

-- | Create a checkbox, also returning the top level \<div\> element.
checkbox'
  :: MonadWidget t m
  => m ()             -- ^ Label contents
  -> CheckboxConfig t -- ^ Checkbox config
  -> m (El t, CheckboxResult t)
checkbox' label CheckboxConfig {..} = do

  (cbEl, _) <- elAttr' "div" divAttrs $ do
    elAttr "input" attrs blank
    el "label" label

  -- Setup the event and callback function for when the value is changed
  (onChangeEvent, onChangeCallback) <- newTriggerEvent
  (onEnableStateEvent, onEnableStateCallback) <- newTriggerEvent
  (onIndeterminateChangeEvent, onIndeterminateChangeCallback) <- newTriggerEvent

  let e = _element_raw cbEl

  -- Activate the checkbox after build and set initial value
  let activate = liftJSM $ do
        activateCheckbox e
          (liftIO . onChangeCallback)
          (liftIO . onEnableStateCallback)
          (liftIO . onIndeterminateChangeCallback)
        suiChecked e _initialValue
        suiEnabled e _initialEnabled
        suiIndeterminate e _initialIndeterminate

  pb <- getPostBuild
  performEvent_ $ activate <$ pb

  -- Allow the value to be set
  performEvent_ $ liftJSM . suiChecked e <$> _setValue
  performEvent_ $ liftJSM . suiEnabled e <$> _setEnabled
  performEvent_ $ liftJSM . suiIndeterminate e <$> _setIndeterminate

  cb <- holdDyn _initialValue onChangeEvent
  enabled <- holdDyn _initialEnabled onEnableStateEvent
  indeterminate <- holdDyn _initialIndeterminate onIndeterminateChangeEvent
  return (cbEl, CheckboxResult
    { _value = cb
    , _change = onChangeEvent
    , _enabled = enabled
    , _indeterminate = indeterminate
    })

  where
    attrs = "type" =: "checkbox" <> "disabled" =: "true" <> _attributes
    classes = mconcat $ "ui checkbox" : map toClassText _types
    alterClasses = maybe (Just $ getClass classes) (\c -> Just $ T.unwords [getClass classes, c])
    divAttrs = M.alter alterClasses "class" _divAttributes

--------------------------------------------------------------------------------
-- Javascript functions

-- | SemanticUI javascript checkbox function
suiCheckbox :: ToJSVal a => a -> JSF
suiCheckbox = js1 ("checkbox" :: Text)

-- | Given a div element, tell semantic-ui to convert it to a checkbox with the
-- given options. The callback function is called on change with the new state.
activateCheckbox
  :: DOM.Element
  -> (Checked -> JSM ())
  -> (Enabled -> JSM ())
  -> (Indeterminate -> JSM ())
  -> JSM ()
activateCheckbox e onChange onEnable onIndeterminate = do
  o <- obj
  o <# ("fireOnInit" :: Text) $ True
  o <# ("onChecked" :: Text) $ fun $ \ _ _ _ -> onChange Checked
  o <# ("onUnchecked" :: Text) $ fun $ \ _ _ _ -> onChange Unchecked
  o <# ("onEnable" :: Text) $ fun $ \ _ _ _ -> onEnable Enabled
  o <# ("onDisable" :: Text) $ fun $ \ _ _ _ -> onEnable Disabled
  o <# ("onDeterminate" :: Text) $ fun $ \ _ _ _ -> onIndeterminate Determinate
  o <# ("onIndeterminate" :: Text) $ fun $ \ _ _ _ -> onIndeterminate Indeterminate
  void $ jQuery e ^. suiCheckbox o

-- | Given an initialised checkbox element, set the state to the given value.
-- The act of setting the state calls any callbacks, so the value stays
-- synchronised.
suiChecked :: DOM.Element -> Checked -> JSM ()
suiChecked e check = void $ jQuery e ^. suiCheckbox arg
  where arg = case check of
          Checked -> "check" :: Text
          Unchecked -> "uncheck"

-- | Given an initialised checkbox element, set the state to the given value.
-- The act of setting the state calls any callbacks, so the value stays
-- synchronised.
suiEnabled :: DOM.Element -> Enabled -> JSM ()
suiEnabled e enabled = void $ jQuery e ^. suiCheckbox arg
  where arg = case enabled of
          Enabled -> "enable" :: Text
          Disabled -> "disable"

-- | Given an initialised checkbox element, set the state to the given value.
-- The act of setting the state calls any callbacks, so the value stays
-- synchronised.
suiIndeterminate :: DOM.Element -> Indeterminate -> JSM ()
suiIndeterminate e determinate = void $
  jQuery e ^. suiCheckbox ("set " <> arg) ^. suiCheckbox arg
  --       ^ workaround upstream issue ^
  --         https://github.com/Semantic-Org/Semantic-UI/issues/5793
  where arg = case determinate of
          Indeterminate -> "indeterminate" :: Text
          Determinate -> "determinate"
