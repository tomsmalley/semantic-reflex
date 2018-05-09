{-# LANGUAGE GADTs #-}

module Reflex.Dom.SemanticUI.Menu where

#ifdef USE_TEMPLATE_HASKELL
import Control.Lens.TH (makeLenses)
#else
import Control.Lens.Type
#endif

import Data.Default (Default (def))
import Data.Semigroup
import Data.Text (Text)
import Reflex
import Reflex.Dom.Core hiding (SetValue)

import Reflex.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data MenuConfig t = MenuConfig
  { _menuConfig_color :: Active t (Maybe Color)
  , _menuConfig_inverted :: Active t Bool
  , _menuConfig_size :: Active t (Maybe Size)
  , _menuConfig_vertical :: Active t Bool
  , _menuConfig_secondary :: Active t Bool
  , _menuConfig_right :: Active t Bool
  , _menuConfig_pointing :: Active t Bool
  , _menuConfig_fluid :: Active t Bool
  , _menuConfig_text :: Active t Bool
  , _menuConfig_compact :: Active t Bool
  , _menuConfig_floated :: Active t (Maybe Floated)
  , _menuConfig_elConfig :: ActiveElConfig t
  }
#ifdef USE_TEMPLATE_HASKELL
makeLenses ''MenuConfig
#endif

instance HasElConfig t (MenuConfig t) where
  elConfig = menuConfig_elConfig

instance Reflex t => Default (MenuConfig t) where
  def = MenuConfig
    { _menuConfig_color = pure Nothing
    , _menuConfig_inverted = pure False
    , _menuConfig_size = pure Nothing
    , _menuConfig_vertical = pure False
    , _menuConfig_secondary = pure False
    , _menuConfig_right = pure False
    , _menuConfig_pointing = pure False
    , _menuConfig_fluid = pure False
    , _menuConfig_text = pure False
    , _menuConfig_compact = pure False
    , _menuConfig_floated = pure Nothing
    , _menuConfig_elConfig = def
    }

menuConfigClasses :: Reflex t => MenuConfig t -> Active t Classes
menuConfigClasses MenuConfig {..} = dynClasses
  [ pure $ Just "ui menu"
  , boolClass "inverted" _menuConfig_inverted
  , fmap toClassText <$> _menuConfig_color
  , fmap toClassText <$> _menuConfig_size
  , boolClass "vertical" $ _menuConfig_vertical
  , boolClass "secondary" _menuConfig_secondary
  , boolClass "right" _menuConfig_right
  , boolClass "pointing" _menuConfig_pointing
  , boolClass "fluid" $ _menuConfig_fluid
  , boolClass "text" $ _menuConfig_text
  , boolClass "compact" $ _menuConfig_compact
  , fmap toClassText <$> _menuConfig_floated
  ]

-- | Create a menu.
menu'
  :: UI t m => MenuConfig t -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
menu' config@MenuConfig {..} widget
  = ui' "div" elConf widget
  where
    elConf = _menuConfig_elConfig <> def
      { _classes = menuConfigClasses config }

-- | Create a menu.
menu :: UI t m => MenuConfig t -> m a -> m a
menu c = fmap snd . menu' c

data MenuLink
  = MenuLink Text -- ^ A real link
  | StyleLink -- ^ A div formatted like a link
  | NoLink    -- ^ Not a link
  deriving (Eq, Show)

data MenuItemConfig t = MenuItemConfig
  { _menuItemConfig_color :: Active t (Maybe Color)
  , _menuItemConfig_disabled :: Active t Bool
  , _menuItemConfig_link :: MenuLink
  , _menuItemConfig_elConfig :: ActiveElConfig t
  }
#ifdef USE_TEMPLATE_HASKELL
makeLenses ''MenuItemConfig
#endif

instance HasElConfig t (MenuItemConfig t) where
  elConfig = menuItemConfig_elConfig

instance Reflex t => Default (MenuItemConfig t) where
  def = MenuItemConfig
    { _menuItemConfig_color = pure Nothing
    , _menuItemConfig_disabled = pure False
    , _menuItemConfig_link = StyleLink
    , _menuItemConfig_elConfig = def
    }

menuItemConfigClasses
  :: Reflex t => MenuItemConfig t -> Active t Classes
menuItemConfigClasses MenuItemConfig {..} = dynClasses
  [ pure $ Just "item"
  , fmap toClassText <$> _menuItemConfig_color
  , boolClass "disabled" _menuItemConfig_disabled
  , boolClass "link" $ pure $ _menuItemConfig_link == StyleLink
  ]

menuItem'
  :: UI t m => MenuItemConfig t -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
menuItem' config widget
  = ui' elTag elConf widget
  where (elTag, elConf) = itemElAttrs config

menuItem :: UI t m => MenuItemConfig t -> m a -> m a
menuItem c = fmap snd . menuItem' c

itemElAttrs :: Reflex t => MenuItemConfig t -> (Text, ActiveElConfig t)
itemElAttrs conf@MenuItemConfig{..} = case _menuItemConfig_link of
  MenuLink href -> ("a", elConf { _attrs = pure $ "href" =: href })
  _ -> ("div", elConf)
  where elConf = _menuItemConfig_elConfig <> def
          { _classes = menuItemConfigClasses conf }

#ifndef USE_TEMPLATE_HASKELL
#include "Menu.th.hs"
#endif
