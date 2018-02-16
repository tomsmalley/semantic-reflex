{-# LANGUAGE TemplateHaskell #-}

-- | Semantic-UI Icon elements
--
-- <https://semantic-ui.com/elements/icon.html>
module Reflex.Dom.SemanticUI.Icon where

import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
import Control.Monad (void)
import Data.Default
import Data.Semigroup ((<>))
import Data.Text (Text)
import Reflex.Dom.Core

import Reflex.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

-- | Icons can be mirrored around an axis
data Flipped = HorizontallyFlipped | VerticallyFlipped
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToClassText Flipped where
  toClassText HorizontallyFlipped = "horizontally flipped"
  toClassText VerticallyFlipped = "vertically flipped"

-- | Icons can be rotated by 90 degrees in either direction
data Rotated = Clockwise | Anticlockwise
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToClassText Rotated where
  toClassText Clockwise = "clockwise rotated"
  toClassText Anticlockwise = "counterclockwise rotated"

-- | Icons can be put into a corner of an 'icons' group
data Corner = TopLeft | TopRight | BottomLeft | BottomRight
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToClassText Corner where
  toClassText TopLeft = "top left corner"
  toClassText TopRight = "top right corner"
  toClassText BottomLeft = "bottom left corner"
  toClassText BottomRight = "bottom right corner"

-- | Config for 'icon's.
data IconConfig t = IconConfig
  { _iconDisabled :: Active t Bool
  -- ^ (default: 'False') Icons can be disabled
  , _iconLoading :: Active t Bool
  -- ^ (default: 'False') Icons can be "loading" (spinning)
  , _iconFitted :: Active t Bool
  -- ^ (default: 'False') Icons can be fitted (no spacing to the sides)
  , _iconLink :: Active t Bool
  -- ^ (default: 'False') Icons can be formatted as a link (causes pointer
  -- cursor on hover)
  , _iconCircular :: Active t Bool
  -- ^ (default: 'False') Icons can have a circular border
  , _iconBordered :: Active t Bool
  -- ^ (default: 'False') Icons can have a square border
  , _iconInverted :: Active t Bool
  -- ^ (default: 'False') Icons can be inverted

  , _iconSize :: Active t (Maybe Size)
  -- ^ (default: 'Nothing') Icons can have a different size
  , _iconFlipped :: Active t (Maybe Flipped)
  -- ^ (default: 'Nothing') Icons can be flipped about an axis
  , _iconRotated :: Active t (Maybe Rotated)
  -- ^ (default: 'Nothing') Icons can be rotated by 90 degrees
  , _iconColor :: Active t (Maybe Color)
  -- ^ (default: 'Nothing') Icons can have a different color
  , _iconCorner :: Active t (Maybe Corner)
  -- ^ (default: 'Nothing') Icons can be placed into a corner (for use inside an
  -- 'icons' element)
  , _iconTitle :: Active t (Maybe Text)
  -- ^ (default: 'Nothing') Convenient way to add a "title" attribute

  , _iconElConfig :: ActiveElConfig t
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''IconConfig

instance HasElConfig t (IconConfig t) where
  elConfig = iconElConfig

instance Reflex t => Default (IconConfig t) where
  def = IconConfig
    { _iconDisabled = pure False
    , _iconLoading = pure False
    , _iconFitted = pure False
    , _iconLink = pure False
    , _iconCircular = pure False
    , _iconBordered = pure False
    , _iconInverted = pure False

    , _iconSize = pure Nothing
    , _iconFlipped = pure Nothing
    , _iconRotated = pure Nothing
    , _iconColor = pure Nothing
    , _iconCorner = pure Nothing
    , _iconTitle = pure Nothing

    , _iconElConfig = def
    }

-- | Make the 'icon' classes.
iconConfigClasses :: Reflex t => IconConfig t -> Active t Classes
iconConfigClasses IconConfig {..} = dynClasses
  [ pure $ Just "icon"

  , boolClass "disabled" _iconDisabled
  , boolClass "loading" _iconLoading
  , boolClass "fitted" _iconFitted
  , boolClass "link" _iconLink
  , boolClass "circular" _iconCircular
  , boolClass "bordered" _iconBordered
  , boolClass "inverted" _iconInverted

  , fmap toClassText . nothingIf Medium <$> _iconSize
  , fmap toClassText <$> _iconFlipped
  , fmap toClassText <$> _iconRotated
  , fmap toClassText <$> _iconColor
  , fmap toClassText <$> _iconCorner
  ]

-- | This is for inclusion in other element configs, fundamentally the same as
-- 'icon'.
data Icon t = Icon (Active t Text) (IconConfig t)

-- | Create an icon. Available icon types are listed here:
-- <https://semantic-ui.com/elements/icon.html>
icon
  :: UI t m
  => Active t Text  -- ^ Icon type
  -> IconConfig t   -- ^ Icon config
  -> m ()
icon i = void . icon' i

-- | Create an icon, returning the 'Element'. Available icon types are listed
-- here: <https://semantic-ui.com/elements/icon.html>
icon'
  :: UI t m => Active t Text -> IconConfig t
  -> m (Element EventResult (DomBuilderSpace m) t)
icon' dynIcon config@IconConfig {..}
  = fst <$> uiElement' "i" elConf blank
  where
    elConf = _iconElConfig <> def
      { _classes = addClass <$> dynIcon <*> iconConfigClasses config
      , _attrs = maybe mempty ("title" =:) <$> _iconTitle
      }

-- | Config for 'icons' groups
data IconsConfig t = IconsConfig
  { _iconsSize :: Active t (Maybe Size)
  -- ^ (default: 'Nothing') Icons can be resized as a group
  , _iconsElConfig :: ActiveElConfig t
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''IconsConfig

instance Reflex t => Default (IconsConfig t) where
  def = IconsConfig
    { _iconsSize = pure Nothing
    , _iconsElConfig = def
    }

-- | Make the 'icons' classes
iconsConfigClasses :: Reflex t => IconsConfig t -> Active t Classes
iconsConfigClasses IconsConfig {..} = dynClasses
  [ pure $ Just "icons"
  , fmap toClassText <$> _iconsSize
  ]

-- | Create an icon group, returning the 'Element'.
icons'
  :: UI t m => IconsConfig t -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
icons' config@IconsConfig {..} = uiElement' "i" elConf
  where
    elConf = _iconsElConfig <> def
      { _classes = iconsConfigClasses config }

-- | Create an icon group.
icons :: UI t m => IconsConfig t -> m a -> m a
icons config = fmap snd . icons' config

