{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Semantic UI segments. Pure reflex implementation is provided.
-- https://semantic-ui.com/collections/segments.html
module Reflex.Dom.SemanticUI.Segment
  (

    segment, segment'
  , SegmentConfig (..)
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

import Control.Lens (makeLenses)
import Data.Default
import Data.Semigroup ((<>))
import Reflex
import Reflex.Dom.Core

import Reflex.Dom.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data Stacked = Stacked | TallStacked | Piled
  deriving (Eq, Show)

instance ToClassText Stacked where
  toClassText Stacked = "stacked"
  toClassText TallStacked = "tall stacked"
  toClassText Piled = "piled"

data SegmentConfig t = SegmentConfig
  { _segmentRaised :: Active t Bool
  -- ^ Segments can be raised
  , _segmentVertical :: Active t Bool
  -- ^ Segments can be formatted as part of a vertical group
  , _segmentInverted :: Active t Bool
  -- ^ Segments can have inverted colors
  , _segmentPadded :: Active t Bool
  -- ^ Segments can have extra padding
  , _segmentCompact :: Active t Bool
  -- ^ If the segment should be compact
  , _segmentCircular :: Active t Bool
  -- ^ Segments can be circular
  , _segmentClearing :: Active t Bool
  -- ^ Segments can clear floated content
  , _segmentBasic :: Active t Bool
  -- ^ A basic segment has no special formatting

  , _segmentStacked :: Active t (Maybe Stacked)
  -- ^ Segments can be stacked
  , _segmentAttached :: Active t (Maybe VerticalAttached)
  -- ^ Segments can be attached vertically
  , _segmentColor :: Active t (Maybe Color)
  -- ^ Segment color
  , _segmentEmphasis :: Active t Emphasis
  -- ^ Segmants can have different emphasis
  , _segmentFloated :: Active t (Maybe Floated)
  -- ^ Segments can be floated
  , _segmentAligned :: Active t (Maybe Aligned)
  -- ^ Segments can have aligned text
  , _segmentSize :: Active t (Maybe Size)
  -- ^ Segments can be a different size

  , _segmentElConfig :: ActiveElConfig t
  -- ^ Config
  }
makeLenses ''SegmentConfig

instance HasElConfig t (SegmentConfig t) where
  elConfig = segmentElConfig

instance Reflex t => Default (SegmentConfig t) where
  def = SegmentConfig
    { _segmentRaised = Static False
    , _segmentStacked = Static Nothing
    , _segmentVertical = Static False
    , _segmentInverted = Static False
    , _segmentAttached = Static Nothing
    , _segmentPadded = Static False
    , _segmentColor = Static Nothing
    , _segmentCompact = Static False
    , _segmentEmphasis = Static Primary
    , _segmentCircular = Static False
    , _segmentClearing = Static False
    , _segmentFloated = Static Nothing
    , _segmentAligned = Static Nothing
    , _segmentBasic = Static False
    , _segmentSize = Static Nothing
    , _segmentElConfig = def
    }

-- | Make the segment div classes from the configuration
segmentConfigClasses :: Reflex t => SegmentConfig t -> Active t Classes
segmentConfigClasses SegmentConfig {..} = activeClasses
  [ Static $ Just "ui segment"
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
segment' config@SegmentConfig{..} = element' "div" elConf
  where
    elConf = _segmentElConfig <> def { _classes = segmentConfigClasses config }

-- | Segment UI Element.
segment :: MonadWidget t m => SegmentConfig t -> m a -> m a
segment config = fmap snd . segment' config

