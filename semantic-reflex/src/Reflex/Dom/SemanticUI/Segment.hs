-- | Semantic UI segments. Pure reflex implementation is provided.
-- https://semantic-ui.com/collections/segments.html
module Reflex.Dom.SemanticUI.Segment
  (

    segments, segments'
  , SegmentsConfig (..)
  , segmentsConfig_horizontal
  , segmentsConfig_raised
  , segmentsConfig_stacked
  , segmentsConfig_compact
  , segmentsConfig_elConfig

  , segment, segment'
  , SegmentConfig (..)
  , Stacked (..)
  , segmentConfig_raised
  , segmentConfig_vertical
  , segmentConfig_inverted
  , segmentConfig_padded
  , segmentConfig_compact
  , segmentConfig_circular
  , segmentConfig_clearing
  , segmentConfig_basic
  , segmentConfig_stacked
  , segmentConfig_attached
  , segmentConfig_color
  , segmentConfig_emphasis
  , segmentConfig_floated
  , segmentConfig_aligned
  , segmentConfig_size
  , segmentConfig_elConfig

  ) where

#ifdef USE_TEMPLATE_HASKELL
import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
#else
import Control.Lens.Type
#endif

import Data.Default
import Data.Semigroup ((<>))
import Reflex
import Reflex.Dom.Core

import Reflex.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data Stacked = Stacked | TallStacked | Piled
  deriving (Eq, Show)

instance ToClassText Stacked where
  toClassText Stacked = "stacked"
  toClassText TallStacked = "tall stacked"
  toClassText Piled = "piled"

data SegmentConfig t = SegmentConfig
  { _segmentConfig_raised :: Active t Bool
  -- ^ Segments can be raised
  , _segmentConfig_vertical :: Active t Bool
  -- ^ Segments can be formatted as part of a vertical group
  , _segmentConfig_inverted :: Active t Bool
  -- ^ Segments can have inverted colors
  , _segmentConfig_padded :: Active t Bool
  -- ^ Segments can have extra padding
  , _segmentConfig_compact :: Active t Bool
  -- ^ If the segment should be compact
  , _segmentConfig_circular :: Active t Bool
  -- ^ Segments can be circular
  , _segmentConfig_clearing :: Active t Bool
  -- ^ Segments can clear floated content
  , _segmentConfig_basic :: Active t Bool
  -- ^ A basic segment has no special formatting

  , _segmentConfig_stacked :: Active t (Maybe Stacked)
  -- ^ Segments can be stacked
  , _segmentConfig_attached :: Active t (Maybe VerticalAttached)
  -- ^ Segments can be attached vertically
  , _segmentConfig_color :: Active t (Maybe Color)
  -- ^ Segment color
  , _segmentConfig_emphasis :: Active t Emphasis
  -- ^ Segmants can have different emphasis
  , _segmentConfig_floated :: Active t (Maybe Floated)
  -- ^ Segments can be floated
  , _segmentConfig_aligned :: Active t (Maybe Aligned)
  -- ^ Segments can have aligned text
  , _segmentConfig_size :: Active t (Maybe Size)
  -- ^ Segments can be a different size

  , _segmentConfig_elConfig :: ActiveElConfig t
  -- ^ Config
  }
#ifdef USE_TEMPLATE_HASKELL
makeLensesWith (lensRules & simpleLenses .~ True) ''SegmentConfig
#endif

instance HasElConfig t (SegmentConfig t) where
  elConfig = segmentConfig_elConfig

instance Reflex t => Default (SegmentConfig t) where
  def = SegmentConfig
    { _segmentConfig_raised = pure False
    , _segmentConfig_stacked = pure Nothing
    , _segmentConfig_vertical = pure False
    , _segmentConfig_inverted = pure False
    , _segmentConfig_attached = pure Nothing
    , _segmentConfig_padded = pure False
    , _segmentConfig_color = pure Nothing
    , _segmentConfig_compact = pure False
    , _segmentConfig_emphasis = pure Primary
    , _segmentConfig_circular = pure False
    , _segmentConfig_clearing = pure False
    , _segmentConfig_floated = pure Nothing
    , _segmentConfig_aligned = pure Nothing
    , _segmentConfig_basic = pure False
    , _segmentConfig_size = pure Nothing
    , _segmentConfig_elConfig = def
    }

-- | Make the segment div classes from the configuration
segmentConfigClasses :: Reflex t => SegmentConfig t -> Active t Classes
segmentConfigClasses SegmentConfig {..} = dynClasses
  [ pure $ Just "ui segment"
  , boolClass "raised" _segmentConfig_raised
  , fmap toClassText <$> _segmentConfig_stacked
  , boolClass "vertical" _segmentConfig_vertical
  , boolClass "inverted" _segmentConfig_inverted
  , fmap toClassText <$> _segmentConfig_attached
  , boolClass "padded" _segmentConfig_padded
  , fmap toClassText <$> _segmentConfig_color
  , boolClass "compact" _segmentConfig_compact
  , Just . toClassText <$> _segmentConfig_emphasis
  , boolClass "circular" _segmentConfig_circular
  , boolClass "clearing" _segmentConfig_clearing
  , fmap toClassText <$> _segmentConfig_floated
  , fmap toClassText <$> _segmentConfig_aligned
  , boolClass "basic" _segmentConfig_basic
  , fmap toClassText <$> _segmentConfig_size
  ]

-- | Segment UI Element.
segment'
  :: UI t m => SegmentConfig t -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
segment' config@SegmentConfig{..} = ui' "div" elConf
  where
    elConf = _segmentConfig_elConfig <> def { _classes = segmentConfigClasses config }

-- | Segment UI Element.
segment :: UI t m => SegmentConfig t -> m a -> m a
segment config = fmap snd . segment' config


data SegmentsConfig t = SegmentsConfig
  { _segmentsConfig_horizontal :: Active t Bool
  -- ^ Segments can be horizontal
  , _segmentsConfig_raised :: Active t Bool
  -- ^ Segments can be raised
  , _segmentsConfig_stacked :: Active t (Maybe Stacked)
  -- ^ Segments can be stacked
  , _segmentsConfig_compact :: Active t Bool
  -- ^ Segments can be compact
  , _segmentsConfig_elConfig :: ActiveElConfig t
  -- ^ Config
  }
#ifdef USE_TEMPLATE_HASKELL
makeLensesWith (lensRules & simpleLenses .~ True) ''SegmentsConfig
#endif

instance HasElConfig t (SegmentsConfig t) where
  elConfig = segmentsConfig_elConfig

instance Reflex t => Default (SegmentsConfig t) where
  def = SegmentsConfig
    { _segmentsConfig_horizontal = pure False
    , _segmentsConfig_raised = pure False
    , _segmentsConfig_stacked = pure Nothing
    , _segmentsConfig_compact = pure False
    , _segmentsConfig_elConfig = def
    }

-- | Make the segment div classes from the configuration
segmentsConfigClasses :: Reflex t => SegmentsConfig t -> Active t Classes
segmentsConfigClasses SegmentsConfig {..} = dynClasses
  [ pure $ Just "ui segments"
  , boolClass "horizontal" _segmentsConfig_horizontal
  , boolClass "raised" _segmentsConfig_raised
  , fmap toClassText <$> _segmentsConfig_stacked
  , boolClass "compact" _segmentsConfig_compact
  ]


-- | Segments UI Element.
segments'
  :: UI t m => SegmentsConfig t -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
segments' config@SegmentsConfig{..} = ui' "div" elConf
  where
    elConf = _segmentsConfig_elConfig <> def { _classes = segmentsConfigClasses config }

-- | Segments UI Element.
segments :: UI t m => SegmentsConfig t -> m a -> m a
segments config = fmap snd . segments' config

#ifndef USE_TEMPLATE_HASKELL
#include "Segment.th.hs"
#endif
