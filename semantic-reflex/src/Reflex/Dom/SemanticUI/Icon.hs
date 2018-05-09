-- | Semantic-UI Icon elements
--
-- <https://semantic-ui.com/elements/icon.html>
module Reflex.Dom.SemanticUI.Icon where

#ifdef USE_TEMPLATE_HASKELL
import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
#else
import Control.Lens.Type
#endif

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
  { _iconConfig_disabled :: Active t Bool
  -- ^ (default: 'False') Icons can be disabled
  , _iconConfig_loading :: Active t Bool
  -- ^ (default: 'False') Icons can be "loading" (spinning)
  , _iconConfig_fitted :: Active t Bool
  -- ^ (default: 'False') Icons can be fitted (no spacing to the sides)
  , _iconConfig_link :: Active t Bool
  -- ^ (default: 'False') Icons can be formatted as a link (causes pointer
  -- cursor on hover)
  , _iconConfig_circular :: Active t Bool
  -- ^ (default: 'False') Icons can have a circular border
  , _iconConfig_bordered :: Active t Bool
  -- ^ (default: 'False') Icons can have a square border
  , _iconConfig_inverted :: Active t Bool
  -- ^ (default: 'False') Icons can be inverted

  , _iconConfig_size :: Active t (Maybe Size)
  -- ^ (default: 'Nothing') Icons can have a different size
  , _iconConfig_flipped :: Active t (Maybe Flipped)
  -- ^ (default: 'Nothing') Icons can be flipped about an axis
  , _iconConfig_rotated :: Active t (Maybe Rotated)
  -- ^ (default: 'Nothing') Icons can be rotated by 90 degrees
  , _iconConfig_color :: Active t (Maybe Color)
  -- ^ (default: 'Nothing') Icons can have a different color
  , _iconConfig_corner :: Active t (Maybe Corner)
  -- ^ (default: 'Nothing') Icons can be placed into a corner (for use inside an
  -- 'icons' element)
  , _iconConfig_title :: Active t (Maybe Text)
  -- ^ (default: 'Nothing') Convenient way to add a "title" attribute

  , _iconConfig_elConfig :: ActiveElConfig t
  }
#ifdef USE_TEMPLATE_HASKELL
makeLensesWith (lensRules & simpleLenses .~ True) ''IconConfig
#endif

instance HasElConfig t (IconConfig t) where
  elConfig = iconConfig_elConfig

instance Reflex t => Default (IconConfig t) where
  def = IconConfig
    { _iconConfig_disabled = pure False
    , _iconConfig_loading = pure False
    , _iconConfig_fitted = pure False
    , _iconConfig_link = pure False
    , _iconConfig_circular = pure False
    , _iconConfig_bordered = pure False
    , _iconConfig_inverted = pure False

    , _iconConfig_size = pure Nothing
    , _iconConfig_flipped = pure Nothing
    , _iconConfig_rotated = pure Nothing
    , _iconConfig_color = pure Nothing
    , _iconConfig_corner = pure Nothing
    , _iconConfig_title = pure Nothing

    , _iconConfig_elConfig = def
    }

-- | Make the 'icon' classes.
iconConfigClasses :: Reflex t => IconConfig t -> Active t Classes
iconConfigClasses IconConfig {..} = dynClasses
  [ pure $ Just "icon"

  , boolClass "disabled" _iconConfig_disabled
  , boolClass "loading" _iconConfig_loading
  , boolClass "fitted" _iconConfig_fitted
  , boolClass "link" _iconConfig_link
  , boolClass "circular" _iconConfig_circular
  , boolClass "bordered" _iconConfig_bordered
  , boolClass "inverted" _iconConfig_inverted

  , fmap toClassText . nothingIf Medium <$> _iconConfig_size
  , fmap toClassText <$> _iconConfig_flipped
  , fmap toClassText <$> _iconConfig_rotated
  , fmap toClassText <$> _iconConfig_color
  , fmap toClassText <$> _iconConfig_corner
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
  = fst <$> ui' "i" elConf blank
  where
    elConf = _iconConfig_elConfig <> def
      { _classes = addClass <$> dynIcon <*> iconConfigClasses config
      , _attrs = maybe mempty ("title" =:) <$> _iconConfig_title
      }

-- | Config for 'icons' groups
data IconsConfig t = IconsConfig
  { _iconsConfig_size :: Active t (Maybe Size)
  -- ^ (default: 'Nothing') Icons can be resized as a group
  , _iconsConfig_elConfig :: ActiveElConfig t
  }
#ifdef USE_TEMPLATE_HASKELL
makeLensesWith (lensRules & simpleLenses .~ True) ''IconsConfig
#endif

instance HasElConfig t (IconsConfig t) where
  elConfig = iconsConfig_elConfig

instance Reflex t => Default (IconsConfig t) where
  def = IconsConfig
    { _iconsConfig_size = pure Nothing
    , _iconsConfig_elConfig = def
    }

-- | Make the 'icons' classes
iconsConfigClasses :: Reflex t => IconsConfig t -> Active t Classes
iconsConfigClasses IconsConfig {..} = dynClasses
  [ pure $ Just "icons"
  , fmap toClassText <$> _iconsConfig_size
  ]

-- | Create an icon group, returning the 'Element'.
icons'
  :: UI t m => IconsConfig t -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
icons' config@IconsConfig {..} = ui' "i" elConf
  where
    elConf = _iconsConfig_elConfig <> def
      { _classes = iconsConfigClasses config }

-- | Create an icon group.
icons :: UI t m => IconsConfig t -> m a -> m a
icons config = fmap snd . icons' config

#ifndef USE_TEMPLATE_HASKELL
#include "Icon.th.hs"
#endif
