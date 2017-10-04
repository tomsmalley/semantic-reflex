{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE RecursiveDo            #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}

-- | Semantic UI checkboxes. Pure reflex implementation is provided.
-- https://semantic-ui.com/modules/checkbox.html
-- TODO:
-- - Read-only and uncheckable states
-- - Blur on escape key
-- - Toggle on enter key
module Reflex.Dom.SemanticUI.Checkbox
  (
  -- * Checkbox
    Checkbox (..)
  -- * Checkbox type
  , CheckboxType (..)
  -- * Checkbox result
  , CheckboxResult (..)
  -- * Checkbox config
  , CheckboxConfig (..)
  ) where

import Control.Lens ((%~))
import Control.Monad ((<=<))
import Data.Default (Default(..))
import Data.Functor.Misc (WrapArg(..))
import Data.Proxy (Proxy(..))
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.HTMLInputElement as Input
import Language.Javascript.JSaddle (liftJSM)
import Reflex
import Reflex.Dom.Core hiding (checkbox, Checkbox, CheckboxConfig)

import Reflex.Dom.SemanticUI.Common

-- | Checkbox types. If you need a radio type, see <RadioGroup>.
data CheckboxType =  Slider | Toggle deriving (Eq, Show)

instance ToClassText CheckboxType where
  toClassText Slider = "slider"
  toClassText Toggle = "toggle"

-- | Configuration of a checkbox. Value and indeterminate are split into initial
-- and set events in order to logically disconnect them from their dynamic
-- return values in CheckboxResult.
data CheckboxConfig t = CheckboxConfig
  { _initialValue :: Bool
  -- ^ Initial value of checkbox (default: False)
  , _setValue :: Event t Bool
  -- ^ Event which sets the value

  , _initialIndeterminate :: Bool
  -- ^ Initial indeterminate state
  , _setIndeterminate :: Event t Bool
  -- ^ Event which sets if the checkbox is indeterminate

  , _altType :: Active t (Maybe CheckboxType)
  -- ^ Checkbox type (e.g. slider)
  , _fitted :: Active t Bool
  -- ^ Checkbox is fitted
  , _disabled :: Active t Bool
  -- ^ Checkbox is disabled
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

-- | Make the checkbox div classes from the configuration
checkboxConfigClasses :: Reflex t => CheckboxConfig t -> Active t ClassText
checkboxConfigClasses CheckboxConfig {..} = mconcat
  [ toClassText <$> _altType
  , memptyUnless "fitted" <$> _fitted
  , memptyUnless "disabled" <$> _disabled
  ]

-- | Result of running a checkbox
data CheckboxResult t = CheckboxResult
  { _value :: Dynamic t Bool
  -- ^ The current checked state
  , _change :: Event t Bool
  -- ^ Changes which are invoked by the user
  , _indeterminate :: Dynamic t Bool
  -- ^ The current indeterminate state
  , _focus :: Dynamic t Bool
  -- ^ The current focused state
  }

instance DynShow t (CheckboxResult t) where
  dynShow CheckboxResult {..} = do
    change <- countWithLast _change
    return $ mconcat
      [ pure "CheckboxResult"
      , (("\n  { _value = " <>) . show) <$> _value
      , (("\n  , _change = " <>) . show) <$> change
      , (("\n  , _indeterminate = " <>) . show) <$> _indeterminate
      , (("\n  , _focus = " <>) . show) <$> _focus
      , pure "\n  }"
      ]

-- | Checkbox UI Element. The minimum useful checkbox only needs a label and a
-- default configuration.
data Checkbox t = Checkbox
  { _label :: Active t Text
  , _config :: CheckboxConfig t
  }

instance UI t m (Checkbox t) where
  type Return t m (Checkbox t) = CheckboxResult t
  ui' (Checkbox label config) = checkbox label config

checkbox
  :: forall t m. MonadWidget t m
  => Active t Text -> CheckboxConfig t
  -> m (Element EventResult (DomBuilderSpace m) t, CheckboxResult t)
checkbox label config@CheckboxConfig {..} = do

  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & initialAttributes .~ constAttrs
        & elementConfig_eventSpec %~ addEventSpecFlags
            (Proxy @(DomBuilderSpace m)) Click (const stopPropagation)

  (divEl, inputEl) <- elActiveAttr' "div" divAttrs $ do
    (inputEl, _) <- element "input" cfg blank
    el "label" $ activeText label
    return inputEl

  let e = DOM.uncheckedCastTo DOM.HTMLInputElement $ _element_raw inputEl

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

  let initialFocus = False -- FIXME: Is this correct?
  focus <- holdDyn initialFocus $ leftmost
    [ False <$ select (_element_events inputEl) (WrapArg Blur)
    , True <$ select (_element_events inputEl) (WrapArg Focus)
    ]

  return $ (divEl, CheckboxResult
    { _value = value
    , _change = fst <$> uiEvent
    , _indeterminate = indeterminate
    , _focus = focus
    })

  where
    divAttrs = mkDivAttrs <$> checkboxConfigClasses config
    mkDivAttrs c = "class" =: getClass ("ui checkbox" <> c)
    constAttrs = "type" =: "checkbox" <> "class" =: "hidden"
