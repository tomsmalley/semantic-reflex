{-# LANGUAGE DeriveFunctor            #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE LambdaCase     #-}

module Reflex.Dom.SemanticUI.Dropdown
  (
    Dropdown (..)

  , DropdownConfig (..)
  , DropdownAction (..)

  , DropdownItem (..)
  , DropdownItemConfig (..)

  , Divider(..)
  ) where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Lens ((^.))
import           Data.Default
import           Data.Functor.Identity
import qualified Data.List as L
import           Data.Map (Map)
import           Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import Data.These
import Data.Align
import qualified GHCJS.DOM.Element as DOM
import           Language.Javascript.JSaddle
import           Text.Read (readMaybe)
import           Reflex
--import           Reflex.Host.Class
import           Reflex.Dom.Core hiding
  ( Dropdown (..), DropdownConfig (..), Select )
------------------------------------------------------------------------------
import           Reflex.Dom.SemanticUI.Common
import           Reflex.Dom.SemanticUI.Icon
import           Reflex.Dom.SemanticUI.Image
import           Reflex.Dom.SemanticUI.Header
------------------------------------------------------------------------------

data DropdownAction
  = Activate
  | Combo
  | Select
  | Hide
  deriving (Eq, Show)

instance ToJSVal DropdownAction where
  toJSVal action = valMakeString $ case action of
    Activate -> "activate"
    Combo -> "combo"
    -- Select doesn't seem to work, so we use activate and prevent the text from
    -- being set in the dropdown element by removing the wrapping "default text"
    -- div around the placeholder.
    Select -> "activate"
    Hide -> "hide"

------------------------------------------------------------------------------

-- | Given a div element, tell semantic-ui to convert it to a dropdown with the
-- given options. The callback function is called on change with the currently
-- selected value.
activateDropdown :: DOM.Element -> Maybe Int -> Bool -> Bool -> DropdownAction
                 -> (Text -> JSM ()) -> JSM ()
activateDropdown e maxSel useLabels fullText dropdownAction onChange = do
  o <- obj
  o <# ("forceSelection" :: Text) $ False
  o <# ("maxSelections" :: Text) $ maxSel
  o <# ("useLabels" :: Text) $ useLabels
  o <# ("fullTextSearch" :: Text) $ fullText
  o <# ("action" :: Text) $ dropdownAction
  o <# ("onChange" :: Text) $ fun $ \_ _ [t, _, _] ->
    onChange =<< fromJSValUnchecked t
  void $ jQuery e ^. js1 ("dropdown" :: Text) o

-- | Given a dropdown element, set the value to the given list. For single
-- dropdowns just provide a singleton list.
dropdownSetExactly :: DOM.Element -> [Int] -> JSM ()
dropdownSetExactly e is
  = void $ jQuery e ^. js2 ("dropdown" :: Text) ("set exactly" :: Text) (map show is)

------------------------------------------------------------------------------

-- | Config for new semantic dropdowns
data DropdownConfig t a = DropdownConfig
  { _initialValue :: a
  , _setValue :: Event t a
  , _attributes :: Map Text Text
  , _placeholder :: Text
  , _maxSelections :: Maybe Int
  , _useLabels :: Bool
  , _fullTextSearch :: Bool
  , _search :: Bool
  , _selection :: Bool
  , _fluid :: Bool
  , _action :: DropdownAction
  , _item :: Bool
  , _textOnly :: Bool
  , _inline :: Bool
  } deriving Functor

-- TODO check that this is lawful
instance Reflex t => Applicative (DropdownConfig t) where
  pure a = DropdownConfig
    { _initialValue = a
    , _setValue = never
    , _attributes = mempty
    , _placeholder = mempty
    , _maxSelections = Nothing
    , _useLabels = True
    , _fullTextSearch = False
    , _search = False
    , _selection = False
    , _fluid = False
    , _action = Activate
    , _item = False
    , _textOnly = False
    , _inline = False
    }
  f <*> a = a
    { _initialValue = (_initialValue f) (_initialValue a)
    , _setValue = fmapMaybe id
        $ fmap (these (const Nothing) (const Nothing) (\f' a' -> Just $ f' a'))
        $ align (_setValue f) (_setValue a)
    }

instance Reflex t => Default (DropdownConfig t (Maybe a)) where
  def = pure Nothing

instance Reflex t => Default (DropdownConfig t [a]) where
  def = pure []

dropdownConfigClasses :: DropdownConfig t a -> [Text]
dropdownConfigClasses DropdownConfig {..} = catMaybes
  [ justWhen _search "search"
  , justWhen _fluid "fluid"
  , justWhen _selection "selection"
  , justWhen _item "item"
  , justWhen _inline "inline"
  ]

-- | Helper function
indexToItem :: [DropdownItem t m a] -> Text -> Maybe a
indexToItem items i' = do
  i <- readMaybe $ T.unpack i'
  getItemAt i items

toValues :: [DropdownItem t m a] -> [a]
toValues [] = []
toValues (item:items) = case item of
  DropdownItem a _ _ -> a : toValues items
  Items _ items' -> toValues items' ++ toValues items
  Content _ -> toValues items

getItemAt :: Int -> [DropdownItem t m a] -> Maybe a
getItemAt i items = toValues items !? i

-- | Custom Dropdown item configuration
data DropdownItemConfig t = DropdownItemConfig
  { _icon :: Maybe (Icon t)
  , _image :: Maybe (Image t)
  , _dataText :: Maybe Text
  , _flag :: Maybe (Flag t)
  }
--  { dataText :: T.Text
--    -- ^ dataText (shown for the selected item)
--  , _
--  }
instance Default (DropdownItemConfig t) where
  def = DropdownItemConfig
    { _icon = Nothing
    , _image = Nothing
    , _dataText = Nothing
    , _flag = Nothing
    }

data DropdownItem t m a where
  DropdownItem :: a -> Text -> (DropdownItemConfig t) -> DropdownItem t m a
  Content :: (UI t m b, ToDropdownItem b) => b -> DropdownItem t m a
  Items :: Text -> [DropdownItem t m a] -> DropdownItem t m a

class ToDropdownItem a where
  toDropdownItem :: a -> a

data Divider = Divider

instance UI t m Divider where
  type Return t m Divider = ()
  ui' Divider = elClass' "div" "divider" blank

instance ToDropdownItem (Header t m a) where
  toDropdownItem (Header size content config) = Header size content $
    config { _header = ContentHeader, _component = False }

instance ToDropdownItem Divider where
  toDropdownItem Divider = Divider
--
-- TODO
-- Selection is incompatible with sub menus.
-- Sections of menu can be scrolling, this is also incompatible with sub menus.
-- Search input in menu
-- Dividers

putItems :: forall t m a. MonadWidget t m => [DropdownItem t m a] -> m ()
putItems items = void $ go 0 items

  where
    _textOnly = False -- TODO FIXME

    go :: Int -> [DropdownItem t m a] -> m Int
    go i = \case

      [] -> return i

      (DropdownItem _ t DropdownItemConfig {..} : rest) -> do
        let attrs = "class" =: "item" <> "data-value" =: tshow i
                <> dataText
            dataText
              | Just dt <- _dataText = "data-text" =: dt
              | _textOnly = "data-text" =: t
              | otherwise = mempty
        elAttr "div" attrs $ do
          maybe blank ui_ _icon
          maybe blank ui_ _image
          maybe blank ui_ _flag
          text t

        go (i + 1) rest

      (Content a : rest) -> ui_ (toDropdownItem a) >> go i rest

      (Items label sub : rest) -> do
        i' <- divClass "item" $ do
          ui_ $ Icon (pure "dropdown") def -- icon must come first for sub dropdowns
          text label
          divClass "menu" $ go i sub
        go i' rest


data Dropdown f t m a = Dropdown
  { _items :: [DropdownItem t m a]
  , _config :: DropdownConfig t (f a)
  }

instance (t ~ t', m ~ m', Eq a) => UI t' m' (Dropdown Maybe t m a) where
  type Return t' m' (Dropdown Maybe t m a) = Dynamic t (Maybe a)
  ui' (Dropdown items config@DropdownConfig{..}) = do
    (e, evt) <- dropdownInternal items False config
    fmap ((,) e) $ holdDyn def $ listToMaybe <$> evt

instance (t ~ t', m ~ m', Eq a) => UI t' m' (Dropdown [] t m a) where
  type Return t' m' (Dropdown [] t m a) = Dynamic t [a]
  ui' (Dropdown items config@DropdownConfig{..}) = do
    (e, evt) <- dropdownInternal items True config
    dynVal <- holdDyn def evt
    return (e, dynVal)

instance (t ~ t', m ~ m', Eq a) => UI t' m' (Dropdown Identity t m a) where
  type Return t' m' (Dropdown Identity t m a) = Dynamic t a
  ui' (Dropdown items config@DropdownConfig{..}) = do
    (e, evt) <- dropdownInternal items False config'
    dynVal <- holdDyn (runIdentity _initialValue) $ fmap f evt
    return (e, dynVal)
      where f (a : _) = a
            f _ = runIdentity _initialValue
            -- Ignore attempts to set the value to an item not in the list
            config' = config { _setValue = ffilter (flip elem (toValues items) . runIdentity) _setValue }

-- | Internal function with shared behaviour
dropdownInternal
  :: forall t m a f. (Foldable f, MonadWidget t m, Eq a)
  => [DropdownItem t m a]             -- ^ Items
  -> Bool                         -- ^ Is multiple dropdown
  -> DropdownConfig t (f a)       -- ^ Dropdown config
  -> m (El t, Event t [a])
dropdownInternal items isMulti conf@DropdownConfig {..} = do

  (divEl, _) <- elAttr' "div" ("class" =: T.unwords classes <> _attributes) $ do

    -- This holds the placeholder. Initial value must be set by js function in
    -- wrapper.
    if _action == Activate
    then divClass "default text" $ text _placeholder
    else text _placeholder -- No wrapper if the text doesn't get replaced by the action
    elAttr "i" ("class" =: "dropdown icon") blank

    -- Dropdown menu
    divClass "menu" $ putItems items

  -- Setup the event and callback function for when the value is changed
  (onChangeEvent, onChangeCallback) <- newTriggerEvent

  pb <- getPostBuild
  -- Activate the dropdown after element is built
  let activate = activateDropdown (_element_raw divEl) maxSel _useLabels _fullTextSearch _action
               $ liftIO . onChangeCallback
  performEvent_ $ liftJSM activate <$ pb
  -- Set initial value
  let setDropdown = liftJSM . dropdownSetExactly (_element_raw divEl)
                  . getIndices
  performEvent_ $ setDropdown _initialValue <$ pb
  -- setValue events
  performEvent_ $ setDropdown <$> _setValue

  let indices = mapMaybe (indexToItem items) . T.splitOn "," <$> onChangeEvent

  return (divEl, indices)

  where
    maxSel = if isMulti then _maxSelections else Nothing
    classes = "ui" : "dropdown" : (if isMulti then "multiple" else "") : dropdownConfigClasses conf
    getIndices :: Foldable f => f a -> [Int]
    getIndices vs = L.findIndices (`elem` vs) $ toValues items
