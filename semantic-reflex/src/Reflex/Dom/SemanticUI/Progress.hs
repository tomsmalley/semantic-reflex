{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

-- | Semantic UI progress module
module Reflex.Dom.SemanticUI.Progress where

#ifdef USE_TEMPLATE_HASKELL
import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
#else
import Control.Lens.Type
#endif

import Control.Lens ((<&>))
import Control.Monad (guard)
import Data.Default
import Data.Foldable (for_)
import Data.Maybe (catMaybes, isNothing)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Reflex
import Reflex.Dom.Core

import qualified Data.Text as T

import Reflex.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

-- | What to render inside the bar itself
data Bar m
  = PercentageBar
  -- ^ Display the percentage
  | FractionDoneBar
  -- ^ Display progress as a fraction of (value / max)
  | Bar (m ())
  -- ^ Display a custom widget. If you wish to use the current value or
  -- percentage, use RecursiveDo to reference the 'Progress' output.

-- | A range holds a minimum and a maximum value
data Range = Range
  { rangeMin :: Int
  , rangeMax :: Int
  } deriving (Eq, Show)

-- | Perecentages
newtype Percent = Percent { unPercent :: Int } deriving (Eq, Ord, Read, Show)
instance Enum Percent where
  fromEnum (Percent p) = p
  toEnum p = if p >= 0 && p <= 100
             then Percent p
             else error "fromEnum: Percent out of range"
instance Bounded Percent where
  minBound = Percent 0
  maxBound = Percent 100

percentText :: Percent -> Text
percentText (Percent p) = tshow p <> "%"

-- | Given a range @(min, max)@ and a value, feature scale the value to a
-- percent scale (0-100) according to the given range. If the range is 0 or
-- either min/max value is negative, returns 'Nothing'.
mkPercent :: Range -> Int -> Maybe Percent
mkPercent (Range vMin vMax) v
  | range <= 0 || vMin < 0 || vMax < 0 = Nothing
  | otherwise = Just $ Percent $ clamp $ 100 * (v - vMin) `div` range
  where range = vMax - vMin
        clamp = min 100 . max 0

-- | Configuration of a progress bar widget.
data ProgressConfig t m = ProgressConfig
  { _progressConfig_bar :: Maybe (Bar m)
  -- ^ (default: 'Nothing') Content inside the bar
  , _progressConfig_label :: Maybe (m ())
  -- ^ (default: 'Nothing') Widget below the bar
  , _progressConfig_indicating :: Dynamic t Bool
  -- ^ (default: 'False') If 'True', changes the bar colour according to the
  -- percentage
  , _progressConfig_active :: Dynamic t Bool
  -- ^ (default: 'False') If 'True', show a pulsing animation on the bar
  , _progressConfig_disabled :: Dynamic t Bool
  -- ^ (default: 'False') If 'True', the progress bar is stylised as being
  -- disabled
  , _progressConfig_indeterminate :: Dynamic t Bool
  -- ^ (default: 'False') If 'True', show the bar in an "indeterminate" state.
  -- The progress bar is hidden and the background is changed to diagonal
  -- stripes. This is automatic when the bar is in a nonsensical state e.g. with
  -- 0 range.
  , _progressConfig_success :: Dynamic t (Maybe Bool)
  -- ^ (default: 'Nothing') If 'Nothing', show the bar in a "success" state when
  -- it reaches 100%. This behavior can be overridden with 'Just': 'True' forces
  -- a "success" state, 'False' forces no "success" state.
  , _progressConfig_warning :: Dynamic t Bool
  -- ^ (default: 'False') If 'True', show the bar in a "warning" state. Takes
  -- CSS precedence over "success".
  , _progressConfig_error :: Dynamic t Bool
  -- ^ (default: 'False') If 'True', show the bar in an "error" state. Takes
  -- CSS precedence over "success" and "warning".
  , _progressConfig_inverted :: Dynamic t Bool
  -- ^ (default: 'False') If 'True', the progress widget will have inverted
  -- colours
  , _progressConfig_attached :: Dynamic t (Maybe VerticalAttached)
  -- ^ (default: 'Nothing') Progress widgets can be vertically attached. This
  -- should only be used when '_progressConfig_bar' and '_progressConfig_label' properties are
  -- 'Nothing'.
  , _progressConfig_size :: Dynamic t (Maybe Size)
  -- ^ (default: 'Nothing') 'Size' of the progress widget. Note: Only sizes
  -- 'Tiny' to 'Big' are supported, values outside the range will be set to the
  -- closest value. Smaller sizes may not be able to fit bar content.
  , _progressConfig_color :: Dynamic t (Maybe Color)
  -- ^ (default: 'Nothing') 'Color' of the progress widget.
  , _progressConfig_minWidth :: Dynamic t Bool
  -- ^ (default: 'True') When 'False', override the "min-width" property of the
  -- bar to remove the limit.
  , _progressConfig_rateLimit :: Maybe NominalDiffTime
  -- ^ (default: 'Just' @0.3@) If enabled, updates which occur faster than
  -- the given time (in seconds) are batched together and applied
  -- simultaneously. This is to allow for smooth animations.
  , _progressConfig_elConfig :: ActiveElConfig t
  -- ^ Underlying element config
  }
#ifdef USE_TEMPLATE_HASKELL
makeLensesWith (lensRules & simpleLenses .~ True) ''ProgressConfig
#endif

instance HasElConfig t (ProgressConfig t m) where
  elConfig = progressConfig_elConfig

instance Reflex t => Default (ProgressConfig t m) where
  def = ProgressConfig
    { _progressConfig_bar = Nothing
    , _progressConfig_label = Nothing
    , _progressConfig_indicating = pure False
    , _progressConfig_active = pure False
    , _progressConfig_disabled = pure False
    , _progressConfig_indeterminate = pure False
    , _progressConfig_success = pure Nothing
    , _progressConfig_warning = pure False
    , _progressConfig_error = pure False
    , _progressConfig_inverted = pure False
    , _progressConfig_attached = pure Nothing
    , _progressConfig_size = pure Nothing
    , _progressConfig_color = pure Nothing
    , _progressConfig_minWidth = pure True
    , _progressConfig_rateLimit = Just 0.3
    , _progressConfig_elConfig = def
    }

-- | Make the progress div classes from the configuration
progressConfigClasses
  :: Reflex t
  => Dynamic t (Maybe Percent) -> ProgressConfig t m
  -> Dynamic t Classes
progressConfigClasses dMaybePercent ProgressConfig {..} = dynClasses'
  [ pure $ Just "ui progress"
  , boolClass "indicating" _progressConfig_indicating
  , boolClass "active" _progressConfig_active
  , boolClass "disabled" _progressConfig_disabled
  , boolClass "warning" _progressConfig_warning
  , boolClass "error" _progressConfig_error
  , boolClass "inverted" _progressConfig_inverted
  , fmap toClassText <$> _progressConfig_attached
  , fmap (toClassText . fixSizes) <$> _progressConfig_size
  , fmap toClassText <$> _progressConfig_color
  , success <$> _progressConfig_success <*> dMaybePercent
  ]
  where
    fixSizes = \case
      Mini -> Tiny
      Huge -> Big
      Massive -> Big
      x -> x
    success (Just x) _ = "success" <$ guard x
    success Nothing p = "success" <$ guard (p == Just maxBound)

-- | Result of running a progress widget.
data Progress t m = Progress
  { _progress_percent :: Dynamic t (Maybe Percent)
  -- ^ The current percentage (can range from 0 to 100)
  , _progress_element :: Element EventResult (DomBuilderSpace m) t
  -- ^ The 'Element' of the wrapping "progress" div
  , _progress_barElement :: Element EventResult (DomBuilderSpace m) t
  -- ^ The 'Element' of the "bar" div
  }
#ifdef USE_TEMPLATE_HASKELL
makeLensesWith (lensRules & simpleLenses .~ True) ''Progress
#endif

instance HasValue (Progress t m) where
  type Value (Progress t m) = Dynamic t (Maybe Percent)
  value = _progress_percent

-- | Display a progress widget, given minimum and maximum values, the initial
-- value, and an 'Event' to update the current value.
progress
  :: UI t m
  => Dynamic t Range      -- ^ Dynamic range
  -> Dynamic t Int        -- ^ Current value
  -> ProgressConfig t m   -- ^ Optional config
  -> m (Progress t m)
progress dRange dValue' config@ProgressConfig{..} = do

  dValue <- case _progressConfig_rateLimit of
    Just t -> rateLimitDyn t dValue'
    Nothing -> pure dValue'

  let dMaybePercent = zipDynWith mkPercent dRange dValue

      dIndeterminate = let f m i = isNothing m || i
                        in zipDynWith f dMaybePercent _progressConfig_indeterminate

      indeterminateStyle = mconcat
        [ "background: repeating-linear-gradient(45deg"
        , ", rgba(0,0,0,0.1), rgba(0,0,0,0.1) 1em"
        , ", rgba(0,0,0,0.2) 1em, rgba(0,0,0,0.2) 2em)"
        ]

      progressConfig = _progressConfig_elConfig <> def
        { _classes = Dyn $ progressConfigClasses dMaybePercent config
        , _attrs = Dyn $ dMaybePercent <&>
          maybe mempty (\(Percent p) -> "data-percent" =: tshow p)
        , _style = Dyn $ dIndeterminate <&>
          \b -> if b then Style indeterminateStyle else mempty
        }

      mkBarStyle indeterminate maybePercent minWidth
        = Style $ T.intercalate ";" $ catMaybes
          [ ffor _progressConfig_rateLimit $ \t -> "transition-duration: " <> tshow t
          , maybePercent <&> \p -> "width:" <> percentText p
          , "visibility: hidden" <$ guard indeterminate
          , "min-width: auto" <$ guard (not minWidth)
          ]

      barConfig = def
        { _classes = "bar"
        , _style = Dyn $ mkBarStyle
          <$> dIndeterminate <*> dMaybePercent <*> _progressConfig_minWidth
        }

  (progressEl, barEl) <- ui' "div" progressConfig $ do
    (barEl, _) <- ui' "div" barConfig $ do
      for_ _progressConfig_bar $ \pContent -> divClass "progress" $ case pContent of
        PercentageBar -> dynText $ maybe mempty percentText <$> dMaybePercent
        FractionDoneBar -> do
          dynText $ tshow <$> dValue
          text $ " / "
          dynText $ tshow . rangeMax <$> dRange
        Bar f -> f

    for_ _progressConfig_label $ divClass "label"

    pure barEl

  pure $ Progress dMaybePercent progressEl barEl

#ifndef USE_TEMPLATE_HASKELL
#include "Progress.th.hs"
#endif
