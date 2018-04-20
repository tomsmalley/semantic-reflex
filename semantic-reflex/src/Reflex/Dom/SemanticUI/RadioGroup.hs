
-- | This module is a work in progress
module Reflex.Dom.SemanticUI.RadioGroup where

{-
import Control.Monad (void)
import Control.Lens (makeLenses)
import Data.Default
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Text (Text)
import Reflex
import Reflex.Dom.Core hiding (SetValue, checkbox)

import Reflex.Dom.SemanticUI.Checkbox
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

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

data RadioGroupConfig t f a = RadioGroupConfig
  { _radioGroupConfigValue :: SetValue t (f a)
  -- ^ Initial value of the radio group. 'Nothing' means no items are selected.
  , _radioGroupType :: Maybe CheckboxType
  -- ^ Checkbox type, e.g. slider
  --, _wrapper :: m DOM.Element -> m DOM.Element
  --, _wrapper :: forall b. m b -> m b
  -- ^ Wrapper around each individual item
  , _radioGroupElConfig :: ActiveElConfig t
  }
makeLenses ''RadioGroupConfig

instance HasElConfig t (RadioGroupConfig t f a) where
  elConfig = radioGroupElConfig

instance (Default (f a), Reflex t)
  => Default (RadioGroupConfig t f a) where
  def = RadioGroupConfig
    { _radioGroupConfigValue = SetValue def Nothing
    , _radioGroupType = Nothing
    , _radioGroupElConfig = def
    }

--------------------------------------------------------------------------------
-- Radio Group Functions

-- | Create a group of radio checkboxes from the given items. The name
-- is required to link the individual checkboxes together, it must be unique to
-- the field. The \<input\> elements are given values automatically by their
-- index.
--
-- https://semantic-ui.com/modules/checkbox.html#radio
radioGroup
  :: forall t m a f. (Monad f, Ord a, MonadWidget t m, Ord (f a), Foldable f)
  => Text -> RadioGroupConfig t f a -> Dynamic t (Map a (m ()))
  -> m (Dynamic t (f a))
radioGroup name conf = fmap snd . radioGroup' name conf

radioGroup'
  :: forall t m a f. (Monad f, Ord a, MonadWidget t m, Ord (f a), Foldable f)
  => Text -> RadioGroupConfig t f a -> Dynamic t (Map a (m ()))
  -> m (El t, Dynamic t (f a))
radioGroup' name _config@RadioGroupConfig{..} items
  = divClass' "grouped fields" $ do

  void $ dyn $ ffor items $ traverse_ $
    divClass "field" . putRadioItem name _radioGroupType
  pure $ pure $ _initial _radioGroupConfigValue

putRadioItem :: MonadWidget t m => Text -> Maybe CheckboxType -> m () -> m ()
putRadioItem name mType m = do
  void $ ui "div" def $ do
    checkbox m $ def
      & checkboxType |~ mType
      & attrs |~ ("name" =: name)
-}

{-
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
