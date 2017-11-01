{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

module Reflex.Dom.SemanticUI.Menu where

import Control.Monad.Reader
import Data.Default (Default (def))
import Data.Semigroup
import qualified Data.Set as S
import Data.Set (Set)
import Data.Text (Text)
import Reflex
import Reflex.Dom.Core hiding (SetValue)

import Reflex.Dom.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data MenuConfig t a = MenuConfig
  { _value :: SetValue t a
  , _size :: Maybe Size
  , _vertical :: Bool
  , _secondary :: Active t Bool
  , _right :: Active t Bool
  , _pointing :: Active t Bool
  , _fluid :: Bool
  , _textContent :: Bool
  , _compact :: Bool
  , _floated :: Maybe Floated
  , _component :: Bool
  , _config :: ActiveElConfig t
  }

mkMenuConfig :: Reflex t => a -> MenuConfig t a
mkMenuConfig a = MenuConfig
  { _value = SetValue a Nothing
  , _size = Nothing
  , _vertical = False
  , _secondary = pure False
  , _right = pure False
  , _pointing = pure False
  , _fluid = False
  , _textContent = False
  , _compact = False
  , _floated = Nothing
  , _component = False
  , _config = def
  }

instance Reflex t => Default (MenuConfig t (Maybe a)) where
  def = mkMenuConfig Nothing

instance Reflex t => Default (MenuConfig t [a]) where
  def = mkMenuConfig []

instance Reflex t => Default (MenuConfig t (Set a)) where
  def = mkMenuConfig S.empty

menuConfigClasses :: Reflex t => MenuConfig t a -> Active t Classes
menuConfigClasses MenuConfig {..} = activeClasses
--  [ justWhen _disabled "disabled"
--  , justWhen _loading "loading"
--  , justWhen _fitted "fitted"
--  , justWhen _link "link"
  [ Static $ Just "menu"
  , boolClass "ui" $ Static $ not _component
  , Static $ toClassText <$> _size
  , boolClass "vertical" $ Static _vertical
  , boolClass "secondary" _secondary
  , boolClass "right" _right
  , boolClass "pointing" _pointing
  , boolClass "fluid" $ Static _fluid
  , boolClass "text" $ Static _textContent
  , boolClass "compact" $ Static _compact
  , Static $ toClassText <$> _floated
--  , uiText <$> _color
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
--  , Static $ Just "link" -- FIXME
  , Static $ toClassText <$> _color
  , boolClass "link" $ Static $ _link == StyleLink
  ]

data MenuItem t m v = forall b. MenuItem v (MenuItemConfig t) (Component Inline m b)

data MenuItem' t m b = MenuItem' (MenuItemConfig t) (Component Inline m b)

itemElAttrs :: Reflex t => MenuItemConfig t -> (Text, ActiveElConfig t)
itemElAttrs conf@MenuItemConfig{..} = case _link of
  MenuLink href -> ("a", elConfig { _attrs = Static $ "href" =: href })
  _ -> ("div", elConfig)
  where elConfig = _config <> def
          { _classes = menuItemConfigClasses conf }


data Menu f t m v b = Menu
  { _config :: MenuConfig t (f v)
  , _items :: Component Menu (ReaderT (Dynamic t (f v)) (EventWriterT t (First v) m)) b
  }

