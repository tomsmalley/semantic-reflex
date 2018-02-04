{-# LANGUAGE TemplateHaskell #-}

-- | Semantic UI segments. Pure reflex implementation is provided.
-- https://semantic-ui.com/collections/segments.html
module Reflex.Dom.SemanticUI.Segment
  (

    segments, segments'
  , SegmentsConfig (..)
  , segmentsHorizontal
  , segmentsRaised
  , segmentsStacked
  , segmentsCompact
  , segmentsElConfig

  , segment, segment'
  , SegmentConfig (..)
  , Stacked (..)
  , segmentRaised
  , segmentVertical
  , segmentInverted
  , segmentPadded
  , segmentCompact
  , segmentCircular
  , segmentClearing
  , segmentBasic
  , segmentStacked
  , segmentAttached
  , segmentColor
  , segmentEmphasis
  , segmentFloated
  , segmentAligned
  , segmentSize
  , segmentElConfig

  ) where

import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
import Data.Default
import Data.Semigroup ((<>))
import Reflex
import Reflex.Dom.Core

import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data Stacked = Stacked | TallStacked | Piled
  deriving (Eq, Show)

instance ToClassText Stacked where
  toClassText Stacked = "stacked"
  toClassText TallStacked = "tall stacked"
  toClassText Piled = "piled"

data SegmentConfig t = SegmentConfig
  { _segmentRaised :: Dynamic t Bool
  -- ^ Segments can be raised
  , _segmentVertical :: Dynamic t Bool
  -- ^ Segments can be formatted as part of a vertical group
  , _segmentInverted :: Dynamic t Bool
  -- ^ Segments can have inverted colors
  , _segmentPadded :: Dynamic t Bool
  -- ^ Segments can have extra padding
  , _segmentCompact :: Dynamic t Bool
  -- ^ If the segment should be compact
  , _segmentCircular :: Dynamic t Bool
  -- ^ Segments can be circular
  , _segmentClearing :: Dynamic t Bool
  -- ^ Segments can clear floated content
  , _segmentBasic :: Dynamic t Bool
  -- ^ A basic segment has no special formatting

  , _segmentStacked :: Dynamic t (Maybe Stacked)
  -- ^ Segments can be stacked
  , _segmentAttached :: Dynamic t (Maybe VerticalAttached)
  -- ^ Segments can be attached vertically
  , _segmentColor :: Dynamic t (Maybe Color)
  -- ^ Segment color
  , _segmentEmphasis :: Dynamic t Emphasis
  -- ^ Segmants can have different emphasis
  , _segmentFloated :: Dynamic t (Maybe Floated)
  -- ^ Segments can be floated
  , _segmentAligned :: Dynamic t (Maybe Aligned)
  -- ^ Segments can have aligned text
  , _segmentSize :: Dynamic t (Maybe Size)
  -- ^ Segments can be a different size

  , _segmentElConfig :: ActiveElConfig t
  -- ^ Config
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''SegmentConfig

instance HasElConfig t (SegmentConfig t) where
  elConfig = segmentElConfig

instance Reflex t => Default (SegmentConfig t) where
  def = SegmentConfig
    { _segmentRaised = pure False
    , _segmentStacked = pure Nothing
    , _segmentVertical = pure False
    , _segmentInverted = pure False
    , _segmentAttached = pure Nothing
    , _segmentPadded = pure False
    , _segmentColor = pure Nothing
    , _segmentCompact = pure False
    , _segmentEmphasis = pure Primary
    , _segmentCircular = pure False
    , _segmentClearing = pure False
    , _segmentFloated = pure Nothing
    , _segmentAligned = pure Nothing
    , _segmentBasic = pure False
    , _segmentSize = pure Nothing
    , _segmentElConfig = def
    }

-- | Make the segment div classes from the configuration
segmentConfigClasses :: Reflex t => SegmentConfig t -> Dynamic t Classes
segmentConfigClasses SegmentConfig {..} = dynClasses
  [ pure $ Just "ui segment"
  , boolClass "raised" _segmentRaised
  , fmap toClassText <$> _segmentStacked
  , boolClass "vertical" _segmentVertical
  , boolClass "inverted" _segmentInverted
  , fmap toClassText <$> _segmentAttached
  , boolClass "padded" _segmentPadded
  , fmap toClassText <$> _segmentColor
  , boolClass "compact" _segmentCompact
  , Just . toClassText <$> _segmentEmphasis
  , boolClass "circular" _segmentCircular
  , boolClass "clearing" _segmentClearing
  , fmap toClassText <$> _segmentFloated
  , fmap toClassText <$> _segmentAligned
  , boolClass "basic" _segmentBasic
  , fmap toClassText <$> _segmentSize
  ]

-- | Segment UI Element.
segment' :: MonadWidget t m => SegmentConfig t -> m a -> m (El t, a)
segment' config@SegmentConfig{..} = uiElement' "div" elConf
  where
    elConf = _segmentElConfig <> def { _classes = segmentConfigClasses config }

-- | Segment UI Element.
segment :: MonadWidget t m => SegmentConfig t -> m a -> m a
segment config = fmap snd . segment' config


data SegmentsConfig t = SegmentsConfig
  { _segmentsHorizontal :: Dynamic t Bool
  -- ^ Segments can be horizontal
  , _segmentsRaised :: Dynamic t Bool
  -- ^ Segments can be raised
  , _segmentsStacked :: Dynamic t (Maybe Stacked)
  -- ^ Segments can be stacked
  , _segmentsCompact :: Dynamic t Bool
  -- ^ Segments can be compact
  , _segmentsElConfig :: ActiveElConfig t
  -- ^ Config
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''SegmentsConfig

instance HasElConfig t (SegmentsConfig t) where
  elConfig = segmentsElConfig

instance Reflex t => Default (SegmentsConfig t) where
  def = SegmentsConfig
    { _segmentsHorizontal = pure False
    , _segmentsRaised = pure False
    , _segmentsStacked = pure Nothing
    , _segmentsCompact = pure False
    , _segmentsElConfig = def
    }

-- | Make the segment div classes from the configuration
segmentsConfigClasses :: Reflex t => SegmentsConfig t -> Dynamic t Classes
segmentsConfigClasses SegmentsConfig {..} = dynClasses
  [ pure $ Just "ui segments"
  , boolClass "horizontal" _segmentsHorizontal
  , boolClass "raised" _segmentsRaised
  , fmap toClassText <$> _segmentsStacked
  , boolClass "compact" _segmentsCompact
  ]


-- | Segments UI Element.
segments' :: MonadWidget t m => SegmentsConfig t -> m a -> m (El t, a)
segments' config@SegmentsConfig{..} = uiElement' "div" elConf
  where
    elConf = _segmentsElConfig <> def { _classes = segmentsConfigClasses config }

-- | Segments UI Element.
segments :: MonadWidget t m => SegmentsConfig t -> m a -> m a
segments config = fmap snd . segments' config

