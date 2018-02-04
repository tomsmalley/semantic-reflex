{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Reflex.Dom.SemanticUI.Menu where

import Control.Lens.TH (makeLenses)
import Data.Default (Default (def))
import Data.Semigroup
import qualified Data.Set as S
import Data.Set (Set)
import Data.Text (Text)
import Reflex
import Reflex.Dom.Core hiding (SetValue)

import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data MenuConfig t a = MenuConfig
  { _menuValue :: SetValue t a
  , _menuColor :: Dynamic t (Maybe Color)
  , _menuInverted :: Dynamic t Bool
  , _menuSize :: Maybe Size
  , _menuVertical :: Bool
  , _menuSecondary :: Dynamic t Bool
  , _menuRight :: Dynamic t Bool
  , _menuPointing :: Dynamic t Bool
  , _menuFluid :: Bool
  , _menuTextContent :: Bool
  , _menuCompact :: Bool
  , _menuFloated :: Maybe Floated
  , _menuComponent :: Bool
  , _menuHighlight :: Bool
  , _menuElConfig :: ActiveElConfig t
  }
makeLenses ''MenuConfig

mkMenuConfig :: Reflex t => a -> MenuConfig t a
mkMenuConfig a = MenuConfig
  { _menuValue = SetValue a Nothing
  , _menuColor = pure Nothing
  , _menuInverted = pure False
  , _menuSize = Nothing
  , _menuVertical = False
  , _menuSecondary = pure False
  , _menuRight = pure False
  , _menuPointing = pure False
  , _menuFluid = False
  , _menuTextContent = False
  , _menuCompact = False
  , _menuFloated = Nothing
  , _menuComponent = False
  , _menuHighlight = False
  , _menuElConfig = def
  }

instance Reflex t => Default (MenuConfig t (Maybe a)) where
  def = mkMenuConfig Nothing

instance Reflex t => Default (MenuConfig t [a]) where
  def = mkMenuConfig []

instance Reflex t => Default (MenuConfig t (Set a)) where
  def = mkMenuConfig S.empty

menuConfigClasses :: Reflex t => MenuConfig t a -> Dynamic t Classes
menuConfigClasses MenuConfig {..} = dynClasses
--  [ justWhen _disabled "disabled"
--  , justWhen _loading "loading"
--  , justWhen _fitted "fitted"
--  , justWhen _link "link"
  [ pure $ Just "menu"
  , boolClass "inverted" _menuInverted
  , fmap toClassText <$> _menuColor
  , boolClass "ui" $ pure $ not _menuComponent
  , pure $ toClassText <$> _menuSize
  , boolClass "vertical" $ pure _menuVertical
  , boolClass "secondary" _menuSecondary
  , boolClass "right" _menuRight
  , boolClass "pointing" _menuPointing
  , boolClass "fluid" $ pure _menuFluid
  , boolClass "text" $ pure _menuTextContent
  , boolClass "compact" $ pure _menuCompact
  , pure $ toClassText <$> _menuFloated
--  , uiText <$> _menuColor
  ]

data MenuLink
  = MenuLink Text -- ^ A real link
  | StyleLink -- ^ A div formatted like a link
  | NoLink    -- ^ Not a link
  deriving (Eq, Show)

data MenuItemConfig t = MenuItemConfig
  { _menuItemColor :: Maybe Color
  , _menuItemLink :: MenuLink
  , _menuItemElConfig :: ActiveElConfig t
  }
makeLenses ''MenuItemConfig

instance Reflex t => Default (MenuItemConfig t) where
  def = MenuItemConfig
    { _menuItemColor = Nothing
    , _menuItemLink = NoLink
    , _menuItemElConfig = def
    }

menuItemConfigClasses :: Reflex t => MenuItemConfig t -> Dynamic t Classes
menuItemConfigClasses MenuItemConfig {..} = dynClasses
  [ pure $ Just "item"
--  , pure $ Just "link" -- FIXME
  , pure $ toClassText <$> _menuItemColor
  , boolClass "link" $ pure $ _menuItemLink == StyleLink
  ]

data MenuItem t m v = forall b. MenuItem v (MenuItemConfig t) (m b)

data MenuItem' t m b = MenuItem' (MenuItemConfig t) (m b)

itemElAttrs :: Reflex t => MenuItemConfig t -> (Text, ActiveElConfig t)
itemElAttrs conf@MenuItemConfig{..} = case _menuItemLink of
  MenuLink href -> ("a", elConf { _attrs = pure $ "href" =: href })
  _ -> ("div", elConf)
  where elConf = _menuItemElConfig <> def
          { _classes = menuItemConfigClasses conf }


{-
data Menu f t m v b = Menu
  { _config :: MenuConfig t (f v)
  , _items :: ReaderT (Dynamic t (f v)) (EventWriterT t (First v) m) b
  }
-}

{-
--------------------------------------------------------------------------------
-- Menu instances

instance ( t ~ t', m ~ m', Ord a, Foldable f, Eq (f a)
         , MonadReader (Dynamic t (f a)) m, EventWriter t (First a) m)
  => Render t' m' (MenuItem t m a) where
  type Return t' m' (MenuItem t m a) = ()
  ui' (MenuItem value config@MenuItemConfig{..} widget) = do
    selected <- ask
    --let isSelected = Dynamic $ demuxed selected $ pure value
    let isSelected = Dynamic $ elem value <$> selected

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
  type Return t' m' (Menu f t m a b) = (Dynamic t (f a), b)
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
         , MonadReader (Dynamic t (f a)) m, EventWriter t (First a) m)
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

