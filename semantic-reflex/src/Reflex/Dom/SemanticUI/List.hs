module Reflex.Dom.SemanticUI.List where

#ifdef USE_TEMPLATE_HASKELL
import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
#else
import Control.Lens.Type
#endif

import Data.Default
import Data.Semigroup ((<>))
import Data.Text (Text)
import Reflex
import Reflex.Dom.Core

import Reflex.Active
import Reflex.Dom.SemanticUI.Common
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
  { _listConfig_type       :: Active t (Maybe ListType)
  , _listConfig_link       :: Active t Bool
  , _listConfig_horizontal :: Active t Bool
  , _listConfig_inverted   :: Active t Bool
  , _listConfig_selection  :: Active t Bool
  , _listConfig_animated   :: Active t Bool
  , _listConfig_relaxed    :: Active t (Maybe Relaxed)
  , _listConfig_divided    :: Active t Bool
  , _listConfig_celled     :: Active t Bool
  , _listConfig_size       :: Active t (Maybe Size)
  , _listConfig_floated    :: Active t (Maybe Floated)
  , _listConfig_aligned    :: Active t (Maybe ListAligned)
  , _listConfig_elConfig   :: ActiveElConfig t
  }
#ifdef USE_TEMPLATE_HASKELL
makeLensesWith (lensRules & simpleLenses .~ True) ''ListConfig
#endif

instance HasElConfig t (ListConfig t) where
  elConfig = listConfig_elConfig

instance Reflex t => Default (ListConfig t) where
  def = ListConfig
    { _listConfig_type = pure Nothing
    , _listConfig_link = pure False
    , _listConfig_horizontal = pure False
    , _listConfig_inverted = pure False
    , _listConfig_selection = pure False
    , _listConfig_animated = pure False
    , _listConfig_relaxed = pure Nothing
    , _listConfig_divided = pure False
    , _listConfig_celled = pure False
    , _listConfig_size = pure Nothing
    , _listConfig_floated = pure Nothing
    , _listConfig_aligned = pure Nothing
    , _listConfig_elConfig = def
    }

listConfigClasses :: Reflex t => ListConfig t -> Active t Classes
listConfigClasses ListConfig {..} = dynClasses
  [ pure $ Just "ui list"
  , fmap toClassText <$> _listConfig_type
  , boolClass "link" _listConfig_link
  , boolClass "horizontal" _listConfig_horizontal
  , boolClass "inverted" _listConfig_inverted
  , boolClass "selection" _listConfig_selection
  , boolClass "animated" _listConfig_animated
  , fmap toClassText <$> _listConfig_relaxed
  , boolClass "divided" _listConfig_divided
  , boolClass "celled" _listConfig_celled
  , fmap toClassText <$> _listConfig_size
  , fmap toClassText <$> _listConfig_floated
  , fmap toClassText <$> _listConfig_aligned
  ]

listItemElementText :: ListItemElement -> Text
listItemElementText ListItemDiv = "div"
listItemElementText ListItemLink = "a"

data ListItemElement = ListItemDiv | ListItemLink

data ListItemConfig t m = ListItemConfig
  { _listItemConfig_preContent :: Maybe (m ())
  , _listItemConfig_element :: ListItemElement
  , _listItemConfig_elConfig :: ActiveElConfig t
  }
#ifdef USE_TEMPLATE_HASKELL
makeLensesWith (lensRules & simpleLenses .~ True) ''ListItemConfig
#endif

instance HasElConfig t (ListItemConfig t m) where
  elConfig = listItemConfig_elConfig

instance Reflex t => Default (ListItemConfig t m) where
  def = ListItemConfig
    { _listItemConfig_preContent = Nothing
    , _listItemConfig_element = ListItemDiv
    , _listItemConfig_elConfig = def
    }

listItemConfigClasses
  :: Reflex t => ListItemConfig t m -> Active t Classes
listItemConfigClasses ListItemConfig {..} = dynClasses
  [ pure $ Just "item"
  ]

-- | Create a list.
--
-- https://semantic-ui.com/elements/list.html
list'
  :: UI t m => ListConfig t -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
list' config@ListConfig {..} widget
  = ui' "div" elConf widget
  where
    elConf = _listConfig_elConfig <> def
      { _classes = listConfigClasses config }

-- | Create a list.
--
-- https://semantic-ui.com/elements/list.html
list :: UI t m => ListConfig t -> m a -> m a
list c = fmap snd . list' c

listItem'
  :: UI t m => ListItemConfig t m -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
listItem' config@ListItemConfig {..} content
  = ui' (listItemElementText _listItemConfig_element) elConf $
    case _listItemConfig_preContent of
      Nothing -> content
      Just m -> m >> divClass "content" content
  where
    elConf = _listItemConfig_elConfig <> def
      { _classes = listItemConfigClasses config }

listItem :: UI t m => ListItemConfig t m -> m a -> m a
listItem c = fmap snd . listItem' c

listHeader' :: UI t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
listHeader' = divClass' "header"

listHeader :: UI t m => m a -> m a
listHeader = fmap snd . listHeader'

listDescription'
  :: UI t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
listDescription' = divClass' "description"

listDescription :: UI t m => m a -> m a
listDescription = fmap snd . listDescription'

#ifndef USE_TEMPLATE_HASKELL
#include "List.th.hs"
#endif
