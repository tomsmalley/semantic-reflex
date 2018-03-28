{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Reflex.Dom.SemanticUI.Menu where

import Control.Lens.TH (makeLenses)
import Data.Default (Default (def))
import Data.Semigroup
import Data.Text (Text)
import Reflex
import Reflex.Dom.Core hiding (SetValue)

import Reflex.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data MenuConfig t = MenuConfig
  { _menuColor :: Active t (Maybe Color)
  , _menuInverted :: Active t Bool
  , _menuSize :: Active t (Maybe Size)
  , _menuVertical :: Active t Bool
  , _menuSecondary :: Active t Bool
  , _menuRight :: Active t Bool
  , _menuPointing :: Active t Bool
  , _menuFluid :: Active t Bool
  , _menuText :: Active t Bool
  , _menuCompact :: Active t Bool
  , _menuFloated :: Active t (Maybe Floated)
  , _menuElConfig :: ActiveElConfig t
  }
makeLenses ''MenuConfig

instance Reflex t => Default (MenuConfig t) where
  def = MenuConfig
    { _menuColor = pure Nothing
    , _menuInverted = pure False
    , _menuSize = pure Nothing
    , _menuVertical = pure False
    , _menuSecondary = pure False
    , _menuRight = pure False
    , _menuPointing = pure False
    , _menuFluid = pure False
    , _menuText = pure False
    , _menuCompact = pure False
    , _menuFloated = pure Nothing
    , _menuElConfig = def
    }

menuConfigClasses :: Reflex t => MenuConfig t -> Active t Classes
menuConfigClasses MenuConfig {..} = dynClasses
  [ pure $ Just "ui menu"
  , boolClass "inverted" _menuInverted
  , fmap toClassText <$> _menuColor
  , fmap toClassText <$> _menuSize
  , boolClass "vertical" $ _menuVertical
  , boolClass "secondary" _menuSecondary
  , boolClass "right" _menuRight
  , boolClass "pointing" _menuPointing
  , boolClass "fluid" $ _menuFluid
  , boolClass "text" $ _menuText
  , boolClass "compact" $ _menuCompact
  , fmap toClassText <$> _menuFloated
  ]

-- | Create a menu.
menu'
  :: UI t m => MenuConfig t -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
menu' config@MenuConfig {..} widget
  = ui' "div" elConf widget
  where
    elConf = _menuElConfig <> def
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
  { _menuItemColor :: Active t (Maybe Color)
  , _menuItemDisabled :: Active t Bool
  , _menuItemLink :: MenuLink
  , _menuItemElConfig :: ActiveElConfig t
  }
makeLenses ''MenuItemConfig

instance Reflex t => Default (MenuItemConfig t) where
  def = MenuItemConfig
    { _menuItemColor = pure Nothing
    , _menuItemDisabled = pure False
    , _menuItemLink = StyleLink
    , _menuItemElConfig = def
    }

menuItemConfigClasses
  :: Reflex t => MenuItemConfig t -> Active t Classes
menuItemConfigClasses MenuItemConfig {..} = dynClasses
  [ pure $ Just "item"
  , fmap toClassText <$> _menuItemColor
  , boolClass "disabled" _menuItemDisabled
  , boolClass "link" $ pure $ _menuItemLink == StyleLink
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
itemElAttrs conf@MenuItemConfig{..} = case _menuItemLink of
  MenuLink href -> ("a", elConf { _attrs = pure $ "href" =: href })
  _ -> ("div", elConf)
  where elConf = _menuItemElConfig <> def
          { _classes = menuItemConfigClasses conf }

{-
data Menu f t m v b = Menu
  { _config :: MenuConfig t (f v)
  , _items :: ReaderT (Active t (f v)) (EventWriterT t (First v) m) b
  }
-}

{-
--------------------------------------------------------------------------------
-- Menu instances

instance ( t ~ t', m ~ m', Ord a, Foldable f, Eq (f a)
         , MonadReader (Active t (f a)) m, EventWriter t (First a) m)
  => Render t' m' (MenuItem t m a) where
  type Return t' m' (MenuItem t m a) = ()
  ui' (MenuItem value config@MenuItemConfig{..} widget) = do
    selected <- ask
    --let isSelected = Active $ demuxed selected $ pure value
    let isSelected = Active $ elem value <$> selected

    (e, _) <- element' "div" (elConfig isSelected) widget
    tellEvent $ First value <$ domEvent Click e
    return (e, ())
      where
--        (_, _) = itemElAttrs config { _link = reLink _link }
--        reLink NoLink = StyleLink
--        reLink a = a
        config' = config & link .~ StyleLink
        elConfig isSelected = _config <> def
          { _classes = addClassMaybe <$> boolClass "active" isSelected
                                     <*> menuItemConfigClasses config'
          }

instance (t ~ t', m ~ m') => Render t' m' (MenuItem' t m b) where
  type Return t' m' (MenuItem' t m b) = b
  ui' (MenuItem' config@MenuItemConfig{..} widget)
    = element' "div" elConfig widget
      where
        elConfig = _config <> def
          { _classes = menuItemConfigClasses $ config & link .~ NoLink }

instance (Selectable f, Ord (f a), Ord a, t ~ t', m ~ m')
  => Render t' m' (Menu f t m a b) where
  type Return t' m' (Menu f t m a b) = (Active t (f a), b)
  ui' (Menu config@MenuConfig{..} items)
    = element' "div" elConfig $ do
    rec
      (b, evt) <- runEventWriterT $ runReaderT items current

      current <- foldDyn mkCurrent (_value ^. initial) $ case _value ^. event of
        Just setValue -> leftmost [Left . getFirst <$> evt, Right <$> setValue]
        Nothing -> Left . getFirst <$> evt

    return (current, b)
    where
      elConfig = _config <> def
        { _classes = menuConfigClasses config }
      mkCurrent :: Either a (f a) -> f a -> f a
      mkCurrent (Left x) acc = selectElement x acc
      mkCurrent (Right acc) _ = acc
-}

{-
instance ( Ord a, m ~ m', t ~ t'
         , MonadReader (Active t (f a)) m, EventWriter t (First a) m)
  => Render t' m' (Menu f t m a b) where
  type Return t' m' (Menu f t m a b) = b
  ui' (Menu config@MenuConfig{..} items) = do
    selected <- ask
    (el, (b, evt)) <- element' "div" elConfig $
      lift $ runEventWriterT $ runReaderT items selected

    tellEvent evt
    return (el, b)

    where elConfig = _config <> def { _classes = menuConfigClasses $ config
                                               & component .~ True }
-}

{-
    unUI $ ui' $ Menu (conf & component .~ True) $ do
      tellEvent
      result <- items
-}
{-
instance (m ~ m', t ~ t') => Render t' m' (Header t m a) where
  type Return t' m' (Header t m a) = a
  ui' (Header conf content) = ui' $ Header conf' content
    where conf' = conf & component .~ True & item .~ True
-}

{-
instance (m ~ m', t ~ t') => Render t' m' (Header t m a) where
  type Return t' m' (Header t m a) = a
  ui' (Header config widget)
    = ui' $ Header (config & component .~ True) widget

instance (t ~ t', m ~ m') => Render t' m' (Anchor t m a) where
  type Return t' m' (Anchor t m a) = AnchorResult t a
  ui' (Anchor contents AnchorConfig{..}) = do
    (e, a) <- element' "a" elConfig contents
    return (e, AnchorResult (domEvent Click e) a)
      where
        elConfig = _config
          & elConfigAttributes %~ (\a -> (maybe id (M.insert "href") <$> _href) <*> a)

-}
-- TODO FIXME For removal:
-- | Force images to appear inline in inline context
{-
instance t ~ t' => Render t' m (Image t) where
  type Return t' m (Image t) = ()
  ui' (Image url conf) = ui' $ Image url $ conf & inline |~ True
-}

