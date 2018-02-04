{-# LANGUAGE TemplateHaskell #-}

module Reflex.Dom.SemanticUI.Label
  (

    label, label'
  , detail, detail'
  , LabelConfig (..)
  , labelImage
  , labelHidden
  , labelBasic
  , labelTag
  , labelFloating
  , labelHorizontal
  , labelAttached
  , labelColor
  , labelPointing
  , labelRibbon
  , labelCorner
  , labelLink
  , labelElConfig

  , Ribbon (..)
  , Pointing (..)
  , TopCorner (..)
  , LabelAttached (..)
  , vertically
  , horizontally

  ) where

import Control.Lens.TH (makeLenses, makeLensesWith, lensRules, simpleLenses)
import Control.Monad (void)
import Data.Default
import Data.Maybe (catMaybes)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core hiding (fromJSString, divClass)

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
  { _vertically :: VerticalAttached
  , _horizontally :: Maybe HorizontalAttached
  }
makeLenses ''LabelAttached

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
  { _labelImage :: Dynamic t Bool
  , _labelHidden :: Dynamic t Bool
  , _labelBasic :: Dynamic t Bool
  , _labelTag :: Dynamic t Bool
  , _labelFloating :: Dynamic t Bool
  , _labelHorizontal :: Dynamic t Bool

  , _labelAttached :: Dynamic t (Maybe LabelAttached)
  , _labelColor :: Dynamic t (Maybe Color)
  , _labelPointing :: Dynamic t (Maybe Pointing)
  , _labelRibbon :: Dynamic t (Maybe Ribbon)
  , _labelCorner :: Dynamic t (Maybe TopCorner)

  , _labelLink :: Bool
  , _labelElConfig :: ActiveElConfig t
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''LabelConfig

instance HasElConfig t (LabelConfig t) where
  elConfig = labelElConfig

instance Reflex t => Default (LabelConfig t) where
  def = LabelConfig
    { _labelAttached = pure Nothing
    , _labelColor = pure Nothing
    , _labelPointing = pure Nothing
    , _labelRibbon = pure Nothing
    , _labelCorner = pure Nothing
    , _labelImage = pure False
    , _labelHidden = pure False
    , _labelBasic = pure False
    , _labelTag = pure False
    , _labelFloating = pure False
    , _labelHorizontal = pure False
    , _labelLink = False
    , _labelElConfig = def
    }

labelConfigClasses :: Reflex t => LabelConfig t -> Dynamic t Classes
labelConfigClasses LabelConfig {..} = dynClasses
  [ pure $ Just "ui label"
  , fmap toClassText <$> _labelAttached
  , fmap toClassText <$> _labelColor
  , fmap toClassText <$> _labelPointing
  , fmap toClassText <$> _labelRibbon
  , fmap toClassText <$> _labelCorner
  , boolClass "hidden" _labelHidden
  , boolClass "basic" _labelBasic
  , boolClass "tag" _labelTag
  , boolClass "floating" _labelFloating
  , boolClass "horizontal" _labelHorizontal
  , boolClass "image" _labelImage
  ]


detail' :: MonadWidget t m => Dynamic t Text -> m (El t)
detail' = fmap fst . uiElement' "div" elConf . dynText
  where elConf = def { _classes = "detail" }

detail :: MonadWidget t m => Dynamic t Text -> m ()
detail = void . detail'

label' :: MonadWidget t m => LabelConfig t -> m a -> m (El t, a)
label' config@LabelConfig {..} = uiElement' elType elConf
  where
    elConf = _labelElConfig <> def
      { _classes = labelConfigClasses config
      }
    elType = if _labelLink then "a" else "div"

label :: MonadWidget t m => LabelConfig t -> m a -> m a
label c = fmap snd . label' c

