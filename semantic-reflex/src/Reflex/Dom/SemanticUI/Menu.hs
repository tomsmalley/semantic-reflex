{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Reflex.Dom.SemanticUI.Menu where

import Control.Monad.Reader
import Data.Default (Default (def))
import Data.Semigroup
import Data.Text (Text)
import Data.These
import Reflex
import Reflex.Dom.Core
import Data.Align

import Reflex.Dom.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data MenuConfig t a = MenuConfig
  { _initialValue :: a
  , _setValue :: Event t a
  , _size :: Maybe Size
  , _vertical :: Bool
  , _fluid :: Bool
  , _textContent :: Bool
  , _compact :: Bool
  , _customMenu :: Maybe Text
  , _floated :: Maybe Floated
  , _config :: ActiveElConfig t
  } deriving Functor

instance Reflex t => Applicative (MenuConfig t) where
  pure a = MenuConfig
    { _initialValue = a
    , _setValue = never
    , _size = Nothing
    , _vertical = False
    , _fluid = False
    , _textContent = False
    , _compact = False
    , _customMenu = Nothing
    , _floated = Nothing
    , _config = def
    }
  MenuConfig { _initialValue = fInit, _setValue = fEvt }
    <*> mca@MenuConfig { _initialValue = aInit, _setValue = aEvt } = mca
      { _initialValue = fInit aInit
      , _setValue = fmapMaybe id
          $ fmap (these (const Nothing) (const Nothing) (\f a -> Just $ f a))
          $ align fEvt aEvt
      }

instance Reflex t => Default (MenuConfig t (Maybe a)) where
  def = pure Nothing

menuConfigClasses :: Reflex t => MenuConfig t a -> Active t Classes
menuConfigClasses MenuConfig {..} = activeClasses
--  [ justWhen _disabled "disabled"
--  , justWhen _loading "loading"
--  , justWhen _fitted "fitted"
--  , justWhen _link "link"
  [ Static $ Just $ "ui menu"
  , Static $ toClassText <$> _size
  , boolClass "vertical" $ Static _vertical
  , boolClass "fluid" $ Static _fluid
  , boolClass "text" $ Static _textContent
  , boolClass "compact" $ Static _compact
  , Static $ toClassText <$> _floated
--  , uiText <$> _color
  , Static $ _customMenu
  ]

data MenuLink
  = MenuLink Text -- ^ A real link
  | StyleLink -- ^ A div formatted like a link
  | NoLink    -- ^ Not a link
  deriving (Eq, Show)

data MenuItemConfig t = MenuItemConfig
  { _color :: Maybe Color
  , _link :: MenuLink
  --, _render :: Maybe (Dynamic t (m ())) -- Extra arbitrary content
  , _config :: ActiveElConfig t
  }

instance Reflex t => Default (MenuItemConfig t) where
  def = MenuItemConfig
    { _color = Nothing
    , _link = NoLink
    , _config = def
    }

menuItemConfigClasses :: Reflex t => MenuItemConfig t -> Active t Classes
menuItemConfigClasses MenuItemConfig {..} = activeClasses
  [ Static $ Just "item"
  , Static $ Just "link" -- FIXME
  , Static $ toClassText <$> _color
  , boolClass "link" $ Static $ _link == StyleLink
  ]

data MenuItem t m v = forall b. MenuItem v (MenuItemConfig t) (Component Inline m b)

itemElAttrs :: Reflex t => MenuItemConfig t -> (Text, ActiveElConfig t)
itemElAttrs conf@MenuItemConfig{..} = case _link of
  MenuLink href -> ("a", elConfig { _attrs = Static $ "href" =: href })
  _ -> ("div", elConfig)
  where elConfig = _config <> def
          { _classes = menuItemConfigClasses conf }


data Menu t m v b = Menu
  { _config :: MenuConfig t (Maybe v)
  , _items :: MonadWidget t m => Component Menu (ReaderT (Demux t (Maybe v))
                                               (EventWriterT t (First v) m)) b
  }

--data MenuDef t m a = MenuDef
--  { _items :: [Component MenuM m a]
--  , _config :: MenuConfig t a
--  }

{-
data Proxy a = Proxy

data MenuItems t m a (xs :: [Type]) where
  -- | Empty menu
  MenuBase :: MenuItems t m a '[]
  -- | Normal clickable menu item
  MenuItem :: a -> Text -> MenuItemConfig t m -> MenuItems t m a xs -> MenuItems t m a xs
  -- | Sub widget, capturing the value
  MenuWidget :: m b -> MenuItemConfig t m -> MenuItems t m a xs -> MenuItems t m a (b ': xs)
  -- | Arbitrary widget, ignoring the value
  MenuWidget_ :: m b -> MenuItemConfig t m -> MenuItems t m a xs -> MenuItems t m a xs
  -- | Sub menu
  SubMenu :: HListAppend ys xs => Dynamic t (m ()) -> MenuItems t m a ys -> MenuItems t m a xs -> MenuItems t m a (ys `Append` xs)
  RightMenu :: HListAppend ys xs => MenuItems t m a ys -> MenuItems t m a xs -> MenuItems t m a (ys `Append` xs)
  -- | Dropdown menu
  DropdownMenu :: Text -> [DropdownItem t m a] -> MenuItems t m a xs -> MenuItems t m a xs

instance (Ord a, m ~ m', t ~ t') => UI t' m' (Menu t m a xs) where
  type Return t' m' (Menu t m a xs) = (Dynamic t (Maybe a), HList xs)
  ui' (Menu items config@MenuConfig {..}) = elWithAnim' "div" attrs $ do
    rec (evts, xs) <- renderItems items vDyn
        vDyn <- holdDyn _initialValue $ leftmost $ _setValue : (fmap Just <$> evts)
    return (vDyn, xs)
    where
      attrs = _config <> def
        { _classes = menuConfigClasses config }

instance (Ord a, m ~ m', t ~ t') => UI t' m' (MenuDef t m a xs) where
  type Return t' m' (MenuDef t m a xs) = (Dynamic t a, HList xs)
  ui' (MenuDef items config@MenuConfig {..}) = elWithAnim' "div" attrs $ do
    rec (evts, xs) <- renderItems items (Just <$> vDyn)
        vDyn <- holdDyn _initialValue $ leftmost $ _setValue : evts
    return (vDyn, xs)
    where
      attrs = _config <> def
        { _classes = menuConfigClasses config }

renderItems
  :: forall t m a xs. (Ord a, Reflex t, MonadWidget t m)
  => MenuItems t m a xs         -- ^ Menu items
  -> Dynamic t (Maybe a)        -- ^ The currently selected value
  -> m ([Event t a], HList xs)
  -- ^ (List of selection events with tagged value, list of captures)
renderItems allItems currentValue = go False allItems
  where
    selected = demux currentValue

    itemElAttrs :: MenuItemConfig t m -> (Text, ActiveElConfig t)
    itemElAttrs conf@MenuItemConfig{..} = case _link of
      MenuLink href -> ("a", attrs { _attrs = Static $ "href" =: href })
      _ -> ("div", attrs)
      where attrs = _config <> def
              { _classes = menuItemConfigClasses conf }

    go :: Bool -> MenuItems t m a ys -> m ([Event t a], HList ys)
    go inDropdown = \case

      MenuBase -> return ([], HNil)

      MenuItem value label conf@MenuItemConfig {..} rest -> do
        clickEvt <- fmap (domEvent Click . fst) $ elWithAnim' elType attrs $ do
            maybe blank ui_ _icon
            text label
            maybe blank ui_ _label
        (evts, hlist) <- go inDropdown rest
        return ((value <$ clickEvt) : evts, hlist)
          where
            (elType, attrs') = itemElAttrs conf { _link = reLink }
            reLink = case _link of
              NoLink -> StyleLink
              a -> a
            isSelected = Dynamic $ demuxed selected $ Just value
            attrs = attrs'
              { _classes = addClassMaybe <$> boolClass "active" isSelected
                                         <*> _classes attrs'
              }

      MenuWidget mb conf rest -> do
        b <- elWithAnim elType attrs mb
        fmap (HCons b) <$> go inDropdown rest
          where (elType, attrs) = itemElAttrs conf

      MenuWidget_ mb conf rest -> do
        elWithAnim elType attrs mb
        go inDropdown rest
          where (elType, attrs) = itemElAttrs conf

      DropdownMenu label items rest -> do
        dynVal <- ui $ Dropdown items $ (def :: DropdownConfig t (Maybe a))
          { _item = True
          -- DropdownMenu does not show the state in the dropdown
          , _action = Hide
          , _placeholder = label
          }
        (restEvents, restList) <- go inDropdown rest
        return (fmapMaybe id (updated dynVal) : restEvents, restList)

      SubMenu mkItem sub rest -> do
        (subEvents, subList) <- divClass "item" $ do
          void $ dyn mkItem
          divClass (T.unwords classes) $ go inDropdown sub
        (restEvents, restList) <- go inDropdown rest
        return (restEvents ++ subEvents, subList `hlistAppend` restList)
          where classes = pure "menu" -- : menuConfigClasses config

      RightMenu sub rest -> do
        (subEvents, subList) <- divClass "right menu" $ go inDropdown sub
        (restEvents, restList) <- go inDropdown rest
        return (restEvents ++ subEvents, subList `hlistAppend` restList)

-}
