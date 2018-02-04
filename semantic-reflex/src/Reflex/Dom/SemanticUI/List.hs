{-# LANGUAGE TemplateHaskell #-}

module Reflex.Dom.SemanticUI.List where

import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
import Data.Default
import Data.Semigroup ((<>))
import Data.Text (Text)
import Reflex
import Reflex.Dom.Core

import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Icon (Icon(Icon), icon)
import Reflex.Dom.SemanticUI.Image (Image(Image), image)
import Reflex.Dom.SemanticUI.Transition

data ListType = Bulleted | Ordered
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToClassText ListType where
  toClassText Bulleted = "bulleted"
  toClassText Ordered = "ordered"

data Relaxed = Relaxed | VeryRelaxed
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToClassText Relaxed where
  toClassText Relaxed = "relaxed"
  toClassText VeryRelaxed = "very relaxed"

data ListAligned = ListTop | ListMiddle | ListBottom
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToClassText ListAligned where
  toClassText ListTop = "top aligned"
  toClassText ListMiddle = "middle aligned"
  toClassText ListBottom = "bottom aligned"

data ListConfig t = ListConfig
  { _listType       :: Dynamic t (Maybe ListType)
  , _listLink       :: Dynamic t Bool
  , _listHorizontal :: Dynamic t Bool
  , _listInverted   :: Dynamic t Bool
  , _listSelection  :: Dynamic t Bool
  , _listAnimated   :: Dynamic t Bool
  , _listRelaxed    :: Dynamic t (Maybe Relaxed)
  , _listDivided    :: Dynamic t Bool
  , _listCelled     :: Dynamic t Bool
  , _listSize       :: Dynamic t (Maybe Size)
  , _listFloated    :: Dynamic t (Maybe Floated)

  , _listAligned    :: Dynamic t (Maybe ListAligned)

  , _listElConfig   :: ActiveElConfig t
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''ListConfig

instance Reflex t => Default (ListConfig t) where
  def = ListConfig
    { _listType = pure Nothing
    , _listLink = pure False
    , _listHorizontal = pure False
    , _listInverted = pure False
    , _listSelection = pure False
    , _listAnimated = pure False
    , _listRelaxed = pure Nothing
    , _listDivided = pure False
    , _listCelled = pure False
    , _listSize = pure Nothing
    , _listFloated = pure Nothing

    , _listAligned = pure Nothing

    , _listElConfig = def
    }

listConfigClasses :: Reflex t => ListConfig t -> Dynamic t Classes
listConfigClasses ListConfig {..} = dynClasses
  [ pure $ Just "ui list"
  , fmap toClassText <$> _listType
  , boolClass "link" _listLink
  , boolClass "horizontal" _listHorizontal
  , boolClass "inverted" _listInverted
  , boolClass "selection" _listSelection
  , boolClass "animated" _listAnimated
  , fmap toClassText <$> _listRelaxed
  , boolClass "divided" _listDivided
  , boolClass "celled" _listCelled
  , fmap toClassText <$> _listSize
  , fmap toClassText <$> _listFloated

  , fmap toClassText <$> _listAligned
  ]

listItemElementText :: ListItemElement -> Text
listItemElementText ListItemDiv = "div"
listItemElementText ListItemLink = "a"

data ListItemElement = ListItemDiv | ListItemLink

data ListItemConfig t = ListItemConfig
  { _listItemIcon :: Maybe (Icon t)
  , _listItemImage :: Maybe (Image t)
  , _listItemElement :: ListItemElement
  , _listItemElConfig :: ActiveElConfig t
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''ListItemConfig

instance HasElConfig t (ListItemConfig t) where
  elConfig = listItemElConfig

instance Reflex t => Default (ListItemConfig t) where
  def = ListItemConfig
    { _listItemIcon = Nothing
    , _listItemImage = Nothing
    , _listItemElement = ListItemDiv
    , _listItemElConfig = def
    }

listItemConfigClasses :: Reflex t => ListItemConfig t -> Dynamic t Classes
listItemConfigClasses ListItemConfig {..} = dynClasses
  [ pure $ Just "item"
  ]

-- | Create a list.
--
-- https://semantic-ui.com/elements/list.html
list' :: MonadWidget t m => ListConfig t -> m a -> m (El t, a)
list' config@ListConfig {..} widget
  = uiElement' "div" elConf widget
  where
    elConf = _listElConfig <> def
      { _classes = listConfigClasses config }

-- | Create a list.
--
-- https://semantic-ui.com/elements/list.html
list :: MonadWidget t m => ListConfig t -> m a -> m a
list c = fmap snd . list' c

listItem' :: MonadWidget t m => ListItemConfig t -> m a -> m (El t, a)
listItem' config@ListItemConfig {..} widget
  = uiElement' (listItemElementText _listItemElement) elConf $
    case _listItemImage of
      Nothing -> case _listItemIcon of
        Nothing -> widget
        Just (Icon i c) -> do
          icon i c
          divClass "content" widget
      Just (Image i c) -> do
        image i c
        divClass "content" widget
  where
    elConf = _listItemElConfig <> def
      { _classes = listItemConfigClasses config }

listItem :: MonadWidget t m => ListItemConfig t -> m a -> m a
listItem c = fmap snd . listItem' c

listHeader' :: MonadWidget t m => m a -> m (El t, a)
listHeader' = divClass' "header"

listHeader :: MonadWidget t m => m a -> m a
listHeader = fmap snd . listHeader'

listDescription' :: MonadWidget t m => m a -> m (El t, a)
listDescription' = divClass' "description"

listDescription :: MonadWidget t m => m a -> m a
listDescription = fmap snd . listDescription'

