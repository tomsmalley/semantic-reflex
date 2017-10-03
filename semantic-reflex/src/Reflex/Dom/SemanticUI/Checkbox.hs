{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE RecursiveDo            #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}

module Reflex.Dom.SemanticUI.Checkbox
  (
  -- * Checkbox
    Checkbox (..)
  , CheckboxType (..)
  -- * Return type
  , CheckboxResult (..)
  -- * Config
  , CheckboxConfig (..)

  ) where

import Control.Monad.Fix (MonadFix)
import           Control.Monad (void, (<=<))
import           Control.Monad.Trans (liftIO)
import           Control.Lens ((^.), (%~))
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

import Data.Proxy
import Data.Functor.Misc
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Event as Event
import GHCJS.DOM.EventM (on)
import qualified GHCJS.DOM.EventM as EventM
import qualified GHCJS.DOM.GlobalEventHandlers as Events
import qualified GHCJS.DOM.HTMLInputElement as Input

import Reflex.Dom.SemanticUI.Common

--------------------------------------------------------------------------------
-- Types

data CheckboxResult t = CheckboxResult
  { _value :: Dynamic t Bool
  , _change :: Event t Bool
  , _indeterminate :: Dynamic t Bool
  }

-- | Checkbox types according to https://semantic-ui.com/modules/checkbox.html.
-- If you need a radio type, see <RadioGroup>.
data CheckboxType =  Slider | Toggle deriving (Eq, Show)

-- | Convert an option to its class representation
instance ToClassText CheckboxType where
  toClassText Slider = "slider"
  toClassText Toggle = "toggle"

data CheckboxConfig t = CheckboxConfig
  { _initialValue :: Bool
  -- ^ Initial value of checkbox (default: False)
  , _setValue :: Event t Bool
  -- ^ Event which sets the value
  , _initialIndeterminate :: Bool
  , _setIndeterminate :: Event t Bool

  , _altType :: Active t (Maybe CheckboxType)
  -- ^ Checkbox type (e.g. slider)
  , _fitted :: Active t Bool
  , _disabled :: Active t Bool
  }

instance Reflex t => Default (CheckboxConfig t) where
  def = CheckboxConfig
    { _initialValue = False
    , _setValue = never
    , _initialIndeterminate = False
    , _setIndeterminate = never

    , _altType = Static Nothing
    , _fitted = Static False
    , _disabled = Static False
    }

checkboxConfigClasses :: Reflex t => CheckboxConfig t -> Active t ClassText
checkboxConfigClasses CheckboxConfig {..} = mconcat
  [ memptyUnless "fitted" <$> _fitted
  , memptyUnless "disabled" <$> _disabled

  , toClassText <$> _altType
  ]


data Checkbox t = Checkbox
  { _label :: Text
  , _config :: CheckboxConfig t
  }

instance UI t m (Checkbox t) where
  type Return t m (Checkbox t) = CheckboxResult t
  ui' (Checkbox label config) = cbinput (text label) config

--------------------------------------------------------------------------------
-- Checkbox Functions

cbinput :: forall t m. MonadWidget t m
        => m () -> CheckboxConfig t
        -> m (Element EventResult (DomBuilderSpace m) t, CheckboxResult t)
cbinput label config@CheckboxConfig {..} = do

  modifyAttrs <- dynamicAttributesToModifyAttributes dynAttrs
  -- (cfg ^. inputElementConfig_elementConfig) $ return ()
  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & initialAttributes .~ constAttrs
        & elementConfig_eventSpec %~ addEventSpecFlags
            (Proxy @(DomBuilderSpace m)) Click (const stopPropagation)
      --  & modifyAttributes .~ modifyAttrs

  (divEl, inputEl) <- elActiveAttr' "div" divAttrs $ do
    (inputEl, _) <- element "input" cfg blank
    el "label" label
    return inputEl

  let e = DOM.uncheckedCastTo DOM.HTMLInputElement $ _element_raw inputEl
      eDiv = DOM.uncheckedCastTo DOM.HTMLElement $ _element_raw divEl

  case _disabled of
    Static d -> Input.setDisabled e d
    Dynamic d -> do
      -- Set initial value
      Input.setDisabled e <=< sample $ current d
      -- Set future values
      performEvent_ $ ffor (updated d) $ Input.setDisabled e

  -- Set initial value
  Input.setChecked e _initialValue
  -- Set future value, filtering uniques
  setEvent <- performEvent $ ffor _setValue $ \newValue -> do
    oldValue <- liftJSM $ Input.getChecked e
    Input.setChecked e newValue
    return $ justWhen (newValue /= oldValue) newValue

  -- Set initial indeterminate
  Input.setIndeterminate e _initialIndeterminate
  -- Set future indeterminate
  performEvent_ $ ffor _setIndeterminate $ Input.setIndeterminate e

  rec

    -- Events from the UI
    let trigger = domEvent Change inputEl <> domEvent Click divEl
    uiEvent <- fmap (fmapMaybe id) $ performEvent $ ffor trigger $ \_ -> do

      -- If the checkbox is disabled we do nothing
      disabled <- Input.getDisabled e
      if disabled
      then return Nothing
      else do

        -- This seems to be the only way to get the values from *before* the
        -- user interacted with the checkbox.
        oldValue <- sample $ current value
        oldIndeterminate <- sample $ current indeterminate

        -- If the checkbox was indeterminate we always set checked to true,
        -- otherwise we toggle the value.
        -- This matches the Semantic UI visual behaviour.
        let newValue = if oldIndeterminate then True else not oldValue
        -- Always clear the indeterminate state
            newIndeterminate = False

        Input.setChecked e newValue
        Input.setIndeterminate e newIndeterminate

        return $ Just (newValue, newIndeterminate)

    indeterminate <- holdDyn _initialIndeterminate $ leftmost
      [ _setIndeterminate
      , snd <$> uiEvent
      ]

    value <- holdUniqDyn <=< holdDyn _initialValue $ leftmost
      [ fmapMaybe id setEvent
      , fst <$> uiEvent
      ]

{-
  let initialFocus = False --TODO: Is this correct?
  hasFocus <- holdDyn initialFocus $ leftmost
    [ False <$ Reflex.select (_element_events e) (WrapArg Blur)
    , True <$ Reflex.select (_element_events e) (WrapArg Focus)
    ]
-}

  return $ (divEl, CheckboxResult
    { _value = value
    , _change = fst <$> uiEvent
    , _indeterminate = indeterminate
    })

  where
    divAttrs = mkDivAttrs <$> checkboxConfigClasses config
    mkDivAttrs c = "class" =: getClass ("ui checkbox" <> c)
    constAttrs = "type" =: "checkbox" <> "class" =: "hidden"
    dynAttrs = mkAttrs <$> active pure id _disabled
    mkAttrs d = constAttrs <> if d then "disabled" =: "disabled" else mempty
