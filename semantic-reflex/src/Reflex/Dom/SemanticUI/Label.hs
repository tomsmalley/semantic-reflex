module Reflex.Dom.SemanticUI.Label
  (

    label, label'
  , detail, detail'
  , LabelConfig (..)
  , labelConfig_image
  , labelConfig_hidden
  , labelConfig_basic
  , labelConfig_tag
  , labelConfig_floating
  , labelConfig_horizontal
  , labelConfig_attached
  , labelConfig_color
  , labelConfig_pointing
  , labelConfig_ribbon
  , labelConfig_corner
  , labelConfig_link
  , labelConfig_elConfig

  , Ribbon (..)
  , Pointing (..)
  , TopCorner (..)
  , LabelAttached (..)
  , labelAttached_vertically
  , labelAttached_horizontally

  ) where

#ifdef USE_TEMPLATE_HASKELL
import Control.Lens.TH (makeLenses, makeLensesWith, lensRules, simpleLenses)
#else
import Control.Lens.Type
#endif

import Control.Monad (void)
import Data.Default
import Data.Maybe (catMaybes)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core hiding (fromJSString, divClass)

import Reflex.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data Pointing = LeftPointing | RightPointing | AbovePointing | BelowPointing
  deriving (Eq, Show)

instance ToClassText Pointing where
  toClassText LeftPointing = "left pointing"
  toClassText RightPointing = "right pointing"
  toClassText AbovePointing = "pointing"
  toClassText BelowPointing = "pointing below" -- Must be in this order

data Ribbon
  = LeftRibbon
  | RightRibbon
  deriving (Eq, Show)

instance ToClassText Ribbon where
  toClassText LeftRibbon = "ribbon"
  toClassText RightRibbon = "right ribbon"

data TopCorner
  = LeftCorner
  | RightCorner
  deriving (Eq, Show)

instance ToClassText TopCorner where
  toClassText LeftCorner = "left corner"
  toClassText RightCorner = "right corner"


-- | If a label is attached, it *must* be vertically attached in some way. There
-- can't be a soley horizontally attached label.
data LabelAttached = LabelAttached
  { _labelAttached_vertically :: VerticalAttached
  , _labelAttached_horizontally :: Maybe HorizontalAttached
  }
#ifdef USE_TEMPLATE_HASKELL
makeLenses ''LabelAttached
#endif

instance Default LabelAttached where
  def = LabelAttached TopAttached Nothing

-- The selectors are very specific for these cases, it must go
--    "[vertical] [horizontal] attached"
instance ToClassText LabelAttached where
  toClassText (LabelAttached v mh)
    = T.unwords $ catMaybes [ vClass v, hClass <$> mh, Just "attached" ]
    where
      vClass TopAttached = Just "top"
      vClass Attached = Nothing
      vClass BottomAttached = Just "bottom"
      hClass LeftAttached = "left"
      hClass RightAttached = "right"

data LabelConfig t = LabelConfig
  { _labelConfig_image :: Active t Bool
  , _labelConfig_hidden :: Active t Bool
  , _labelConfig_basic :: Active t Bool
  , _labelConfig_tag :: Active t Bool
  , _labelConfig_floating :: Active t Bool
  , _labelConfig_horizontal :: Active t Bool

  , _labelConfig_attached :: Active t (Maybe LabelAttached)
  , _labelConfig_color :: Active t (Maybe Color)
  , _labelConfig_pointing :: Active t (Maybe Pointing)
  , _labelConfig_ribbon :: Active t (Maybe Ribbon)
  , _labelConfig_corner :: Active t (Maybe TopCorner)

  , _labelConfig_link :: Bool
  , _labelConfig_elConfig :: ActiveElConfig t
  }
#ifdef USE_TEMPLATE_HASKELL
makeLensesWith (lensRules & simpleLenses .~ True) ''LabelConfig
#endif

instance HasElConfig t (LabelConfig t) where
  elConfig = labelConfig_elConfig

instance Reflex t => Default (LabelConfig t) where
  def = LabelConfig
    { _labelConfig_attached = pure Nothing
    , _labelConfig_color = pure Nothing
    , _labelConfig_pointing = pure Nothing
    , _labelConfig_ribbon = pure Nothing
    , _labelConfig_corner = pure Nothing
    , _labelConfig_image = pure False
    , _labelConfig_hidden = pure False
    , _labelConfig_basic = pure False
    , _labelConfig_tag = pure False
    , _labelConfig_floating = pure False
    , _labelConfig_horizontal = pure False
    , _labelConfig_link = False
    , _labelConfig_elConfig = def
    }

labelConfigClasses :: Reflex t => LabelConfig t -> Active t Classes
labelConfigClasses LabelConfig {..} = dynClasses
  [ pure $ Just "ui label"
  , fmap toClassText <$> _labelConfig_attached
  , fmap toClassText <$> _labelConfig_color
  , fmap toClassText <$> _labelConfig_pointing
  , fmap toClassText <$> _labelConfig_ribbon
  , fmap toClassText <$> _labelConfig_corner
  , boolClass "hidden" _labelConfig_hidden
  , boolClass "basic" _labelConfig_basic
  , boolClass "tag" _labelConfig_tag
  , boolClass "floating" _labelConfig_floating
  , boolClass "horizontal" _labelConfig_horizontal
  , boolClass "image" _labelConfig_image
  ]


detail'
  :: UI t m => Dynamic t Text -> m (Element EventResult (DomBuilderSpace m) t)
detail' = fmap fst . ui' "div" elConf . dynText
  where elConf = def { _classes = "detail" }

detail :: UI t m => Dynamic t Text -> m ()
detail = void . detail'

label'
  :: UI t m => LabelConfig t -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
label' config@LabelConfig {..} = ui' elType elConf
  where
    elConf = _labelConfig_elConfig <> def
      { _classes = labelConfigClasses config
      }
    elType = if _labelConfig_link then "a" else "div"

label :: UI t m => LabelConfig t -> m a -> m a
label c = fmap snd . label' c

#ifndef USE_TEMPLATE_HASKELL
#include "Label.th.hs"
#endif
