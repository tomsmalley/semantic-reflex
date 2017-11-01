{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Reflex.Dom.SemanticUI.List where

import Data.Default
import Data.Text (Text)
import Reflex

import Reflex.Dom.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Icon (Icon)
import Reflex.Dom.SemanticUI.Image (Image)
import Reflex.Dom.SemanticUI.Transition hiding (divClass)

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
  { _listType   :: Active t (Maybe ListType)
  , _link       :: Active t Bool
  , _horizontal :: Active t Bool
  , _inverted   :: Active t Bool
  , _selection  :: Active t Bool
  , _animated   :: Active t Bool
  , _relaxed    :: Active t (Maybe Relaxed)
  , _divided    :: Active t Bool
  , _celled     :: Active t Bool
  , _size       :: Active t (Maybe Size)
  , _floated    :: Active t (Maybe Floated)

  , _aligned    :: Active t (Maybe ListAligned)

  , _config     :: ActiveElConfig t
  }

instance Reflex t => Default (ListConfig t) where
  def = ListConfig
    { _listType = pure Nothing
    , _link = pure False
    , _horizontal = pure False
    , _inverted = pure False
    , _selection = pure False
    , _animated = pure False
    , _relaxed = pure Nothing
    , _divided = pure False
    , _celled = pure False
    , _size = pure Nothing
    , _floated = pure Nothing

    , _aligned = pure Nothing

    , _config = def
    }

listConfigClasses :: Reflex t => ListConfig t -> Active t Classes
listConfigClasses ListConfig {..} = activeClasses
  [ Static $ Just "ui list"
  , fmap toClassText <$> _listType
  , boolClass "link" _link
  , boolClass "horizontal" _horizontal
  , boolClass "inverted" _inverted
  , boolClass "selection" _selection
  , boolClass "animated" _animated
  , fmap toClassText <$> _relaxed
  , boolClass "divided" _divided
  , boolClass "celled" _celled
  , fmap toClassText <$> _size
  , fmap toClassText <$> _floated

  , fmap toClassText <$> _aligned
  ]

-- | Create a list.
--
-- https://semantic-ui.com/elements/list.html
data List t m a = List
  { _config :: ListConfig t
  , _content :: Component List m a
  }

listItemElement :: ListItemElement -> Text
listItemElement ListItemDiv = "div"
listItemElement ListItemLink = "a"

data ListItemElement = ListItemDiv | ListItemLink

data ListItemConfig t = ListItemConfig
  { _icon :: Maybe (Icon t)
  , _image :: Maybe (Image t)
  , _as :: ListItemElement
  , _config :: ActiveElConfig t
  }

instance Reflex t => Default (ListItemConfig t) where
  def = ListItemConfig
    { _icon = Nothing
    , _image = Nothing
    , _as = ListItemDiv
    , _config = def
    }

listItemConfigClasses :: Reflex t => ListItemConfig t -> Active t Classes
listItemConfigClasses ListItemConfig {..} = activeClasses
  [ Static $ Just "item"
  ]

data ListItem t m a = ListItem
  { _config :: ListItemConfig t
  , _content :: Component ListItem m a
  }

-- listHeader :: Component Inline m a -> Component ListItem m a

newtype ListHeader t m a = ListHeader
  { _content :: Component Inline m a
  }

newtype ListDescription t m a = ListDescription
  { _content :: Component Inline m a
  }
