{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeFamilies             #-}

module Reflex.Dom.SemanticUI.RadioGroup
  (
  {-
  -- * Radio Group
    RadioGroup (..)
  , RadioGroupConfig (..)
  -- * Radio Item
  , RadioItem (..)
  , RadioItemConfig (..)
  -}
  ) where

import           Control.Monad (void)
import           Control.Monad.Trans
import           Control.Lens ((^.))
import           Data.Default
import qualified Data.List as L
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (isJust)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified GHCJS.DOM.Element as DOM
import           Language.Javascript.JSaddle
import           Reflex
import           Reflex.Dom.Core hiding
  ()

import Reflex.Dom.SemanticUI.Checkbox (CheckboxType (..))
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

{-
--------------------------------------------------------------------------------
-- Radio Item

data RadioItemConfig = RadioItemConfig
  { _inputAttributes :: Map Text Text
  -- ^ Attributes of the \<input\> element
  , _divAttributes :: Map Text Text
  -- ^ Attributes of the wrapping \<div\> element
  }

instance Default RadioItemConfig where
  def = RadioItemConfig
    { _inputAttributes = mempty
    , _divAttributes = mempty
    }

data RadioItem a = RadioItem
  { _value :: a
  -- ^ Item value
  , _label :: Text
  -- ^ Widget inside the \<label\> element
  , _config :: RadioItemConfig
  -- ^ Item config
  }

--------------------------------------------------------------------------------
-- Radio Group Config

data RadioGroupConfig t a = RadioGroupConfig
  { _initialValue :: Maybe a
  -- ^ Initial value of the radio group. 'Nothing' means no items are selected.
  , _setValue :: Event t (Maybe a)
  -- ^ Event which sets the value. 'Nothing' clears the selection.
  , _altType :: Maybe CheckboxType
  -- ^ Checkbox type, e.g. slider
  --, _wrapper :: m DOM.Element -> m DOM.Element
  --, _wrapper :: forall b. m b -> m b
  -- ^ Wrapper around each individual item
  , _config :: ActiveElConfig t
  }

radioGroupClasses :: Reflex t => RadioGroupConfig t a -> Active t Classes
radioGroupClasses RadioGroupConfig {..} = activeClasses
  [ ]

instance Reflex t => Default (RadioGroupConfig t a) where
  def = RadioGroupConfig
    { _initialValue = Nothing
    , _setValue = never
    , _altType = Nothing
    --, _wrapper = divClass "field"
    , _config = def
    }

--------------------------------------------------------------------------------
-- Radio Group Functions

data RadioGroup t a = RadioGroup
  { _name :: Text
  , _items :: [RadioItem a]
  , _config :: RadioGroupConfig t a
  }

instance Eq a => UI t m (RadioGroup t a) where
  type Return t m (RadioGroup t a) = Dynamic t (Maybe a)
  ui' (RadioGroup name items config) = radioGroup name items config

-- | Create a group of radio checkboxes from the given list of items. The name
-- is required to link the individual checkboxes together, it must be unique to
-- the field. The \<input\> elements are given values automatically by their
-- index.
--
-- https://semantic-ui.com/modules/checkbox.html#radio
radioGroup
  :: (Eq a, MonadWidget t m)
  => Text                   -- ^ Name of \<input\> elements
  -> [RadioItem a]      -- ^ Items
  -> RadioGroupConfig t a -- ^ Group config
  -> m (El t, Dynamic t (Maybe a))
radioGroup name items config = elClass' "div" "grouped fields" $ do

  -- Insert all of the items, collecting the raw elements and wrapping them with
  -- the given wrapper function
  inputEls <- traverse (divClass "field") . imap (putRadioItem name classes) $ items

  -- Helper to lookup the index of an item
  let getIndex v = L.findIndex ((==) v . _value) items
      setRadio = liftJSM . setRadioGroup inputEls . (getIndex =<<)

  -- Event performed when user fires a set value event
  onSetEvent <- performEvent $ setRadio <$> _setValue config

  -- Set initial value
  pb <- getPostBuild
  setInitialEvent <- performEvent $ setRadio initialValue <$ pb

  -- On change callbacks
  (onChangeEvent, onChangeCallback) <- newTriggerEvent
  let setupCallbacks = liftJSM . setRadioCallbacks inputEls
                     $ liftIO . onChangeCallback
  performEvent_ $ setupCallbacks <$ pb

  index <- holdDyn Nothing $ leftmost [onChangeEvent, onSetEvent, setInitialEvent]
  return $ (\i -> fmap _value $ (items !?) =<< i) <$> index

  where
    initialValue = _initialValue config
    -- Detect clashing classes: toggle and slider take precedence over radio
    classes = "ui checkbox " <> maybe "radio" toClassText (_altType config)

-- | Make an individual radio checkbox item.
putRadioItem
  :: MonadWidget t m
  => Text                 -- ^ HTML name attribute
  -> Text                 -- ^ Classes for the enclosing \<div\> element
  -> Int                  -- ^ Value of item
  -> RadioItem a  -- ^ Item configuration
  -> m DOM.Element
putRadioItem name classes i (RadioItem _ label (RadioItemConfig attrs' divAttrs')) = do

  -- Make the radio item
  (cbEl, inputEl) <- elAttr' "div" divAttrs $ do
    (inputEl, _) <- elAttr' "input" attrs blank
    el "label" $ text label
    return inputEl

  -- Setup radio checkbox element with semantic ui
  pb <- getPostBuild
  performEvent_ $ (liftJSM $ activateRadio $ _element_raw cbEl) <$ pb

  return $ _element_raw inputEl

  where
    attrs = "type" =: "radio" <> "value" =: tshow i <> "name" =: name <> attrs'
    divAttrs = M.alter alterClasses "class" divAttrs'
    alterClasses = maybe (Just classes) (\c -> Just $ T.unwords [classes, c])

--------------------------------------------------------------------------------
-- Javascript Functions

-- | Activate a radio element with Semantic UI. No callbacks by semantic ui
-- because they don't notify when a radio is automatically de-selected, so
-- instead we manually put on change listeners to the individual radio items.
activateRadio :: DOM.Element -> JSM ()
activateRadio e = void $ jQuery e ^. js0 ("checkbox" :: Text)

-- | Given a list of radio checkboxes, setup onChange callbacks
setRadioCallbacks :: [DOM.Element] -> (Maybe Int -> JSM ()) -> JSM ()
setRadioCallbacks es onChange = do
  let checked = jQuery es
        ^. js1 ("filter" :: Text) (":checked" :: Text)
        ^. js0 ("val" :: Text)
  let callback = fun $ \_ _ _ -> onChange =<< fromJSValUnchecked =<< checked
  void $ jQuery es ^. js2 ("on" :: Text) ("change" :: Text) callback

-- | Set the current value of a radio group.
setRadioGroup :: [DOM.Element] -> Maybe Int -> JSM (Maybe Int)
setRadioGroup es Nothing
  = Nothing <$ jQuery es ^. js2 ("prop" :: Text) ("checked" :: Text) False
setRadioGroup es (Just v) = do
  void $ jQuery es
    ^. js1 ("filter" :: Text) ("[value=" <> tshow v <> "]")
    ^. js2 ("prop" :: Text) ("checked" :: Text) True
  -- Try to prevent state being out of sync by returning which is selected
  selected <- jQuery es
    ^. js1 ("filter" :: Text) (":checked" :: Text)
    ^. js0 ("val" :: Text)
  syncPoint -- needed for the initial set event to fire
  fromJSValUnchecked selected

-}
