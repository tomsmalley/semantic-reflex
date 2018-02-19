{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

-- | Semantic UI progress module
module Reflex.Dom.SemanticUI.Progress where

import Control.Lens ((<&>))
import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
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
  { _progressBar :: Maybe (Bar m)
  -- ^ (default: 'Nothing') Content inside the bar
  , _progressLabel :: Maybe (m ())
  -- ^ (default: 'Nothing') Widget below the bar
  , _progressIndicating :: Dynamic t Bool
  -- ^ (default: 'False') If 'True', changes the bar colour according to the
  -- percentage
  , _progressActive :: Dynamic t Bool
  -- ^ (default: 'False') If 'True', show a pulsing animation on the bar
  , _progressDisabled :: Dynamic t Bool
  -- ^ (default: 'False') If 'True', the progress bar is stylised as being
  -- disabled
  , _progressIndeterminate :: Dynamic t Bool
  -- ^ (default: 'False') If 'True', show the bar in an "indeterminate" state.
  -- The progress bar is hidden and the background is changed to diagonal
  -- stripes. This is automatic when the bar is in a nonsensical state e.g. with
  -- 0 range.
  , _progressSuccess :: Dynamic t (Maybe Bool)
  -- ^ (default: 'Nothing') If 'Nothing', show the bar in a "success" state when
  -- it reaches 100%. This behavior can be overridden with 'Just': 'True' forces
  -- a "success" state, 'False' forces no "success" state.
  , _progressWarning :: Dynamic t Bool
  -- ^ (default: 'False') If 'True', show the bar in a "warning" state. Takes
  -- CSS precedence over "success".
  , _progressError :: Dynamic t Bool
  -- ^ (default: 'False') If 'True', show the bar in an "error" state. Takes
  -- CSS precedence over "success" and "warning".
  , _progressInverted :: Dynamic t Bool
  -- ^ (default: 'False') If 'True', the progress widget will have inverted
  -- colours
  , _progressAttached :: Dynamic t (Maybe VerticalAttached)
  -- ^ (default: 'Nothing') Progress widgets can be vertically attached. This
  -- should only be used when '_progressBar' and '_progressLabel' properties are
  -- 'Nothing'.
  , _progressSize :: Dynamic t (Maybe Size)
  -- ^ (default: 'Nothing') 'Size' of the progress widget. Note: Only sizes
  -- 'Tiny' to 'Big' are supported, values outside the range will be set to the
  -- closest value. Smaller sizes may not be able to fit bar content.
  , _progressColor :: Dynamic t (Maybe Color)
  -- ^ (default: 'Nothing') 'Color' of the progress widget.
  , _progressMinWidth :: Dynamic t Bool
  -- ^ (default: 'True') When 'False', override the "min-width" property of the
  -- bar to remove the limit.
  , _progressRateLimit :: Maybe NominalDiffTime
  -- ^ (default: 'Just' @0.3@) If enabled, updates which occur faster than
  -- the given time (in seconds) are batched together and applied
  -- simultaneously. This is to allow for smooth animations.
  , _progressElConfig :: ActiveElConfig t
  -- ^ Underlying element config
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''ProgressConfig

instance HasElConfig t (ProgressConfig t m) where
  elConfig = progressElConfig

instance Reflex t => Default (ProgressConfig t m) where
  def = ProgressConfig
    { _progressBar = Nothing
    , _progressLabel = Nothing
    , _progressIndicating = pure False
    , _progressActive = pure False
    , _progressDisabled = pure False
    , _progressIndeterminate = pure False
    , _progressSuccess = pure Nothing
    , _progressWarning = pure False
    , _progressError = pure False
    , _progressInverted = pure False
    , _progressAttached = pure Nothing
    , _progressSize = pure Nothing
    , _progressColor = pure Nothing
    , _progressMinWidth = pure True
    , _progressRateLimit = Just 0.3
    , _progressElConfig = def
    }

-- | Make the progress div classes from the configuration
progressConfigClasses
  :: Reflex t
  => Dynamic t (Maybe Percent) -> ProgressConfig t m
  -> Dynamic t Classes
progressConfigClasses dMaybePercent ProgressConfig {..} = dynClasses'
  [ pure $ Just "ui progress"
  , boolClass "indicating" _progressIndicating
  , boolClass "active" _progressActive
  , boolClass "disabled" _progressDisabled
  , boolClass "warning" _progressWarning
  , boolClass "error" _progressError
  , boolClass "inverted" _progressInverted
  , fmap toClassText <$> _progressAttached
  , fmap (toClassText . fixSizes) <$> _progressSize
  , fmap toClassText <$> _progressColor
  , success <$> _progressSuccess <*> dMaybePercent
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
  { _progressPercent :: Dynamic t (Maybe Percent)
  -- ^ The current percentage (can range from 0 to 100)
  , _progressElement :: Element EventResult (DomBuilderSpace m) t
  -- ^ The 'Element' of the wrapping "progress" div
  , _progressBarElement :: Element EventResult (DomBuilderSpace m) t
  -- ^ The 'Element' of the "bar" div
  }

-- | Display a progress widget, given minimum and maximum values, the initial
-- value, and an 'Event' to update the current value.
progress
  :: UI t m
  => Dynamic t Range      -- ^ Dynamic range
  -> Dynamic t Int        -- ^ Current value
  -> ProgressConfig t m   -- ^ Optional config
  -> m (Progress t m)
progress dRange dValue' config@ProgressConfig{..} = do

  dValue <- case _progressRateLimit of
    Just t -> rateLimitDyn t dValue'
    Nothing -> pure dValue'

  let dMaybePercent = zipDynWith mkPercent dRange dValue

      dIndeterminate = let f m i = isNothing m || i
                        in zipDynWith f dMaybePercent _progressIndeterminate

      indeterminateStyle = "background: repeating-linear-gradient(45deg\
        \, rgba(0,0,0,0.1), rgba(0,0,0,0.1) 1em\
        \, rgba(0,0,0,0.2) 1em, rgba(0,0,0,0.2) 2em)"

      progressConfig = _progressElConfig <> def
        { _classes = Dyn $ progressConfigClasses dMaybePercent config
        , _attrs = Dyn $ dMaybePercent <&>
          maybe mempty (\(Percent p) -> "data-percent" =: tshow p)
        , _style = Dyn $ dIndeterminate <&>
          \b -> if b then Style indeterminateStyle else mempty
        }

      mkBarStyle indeterminate maybePercent minWidth
        = Style $ T.intercalate ";" $ catMaybes
          [ ffor _progressRateLimit $ \t -> "transition-duration: " <> tshow t
          , maybePercent <&> \p -> "width:" <> percentText p
          , "visibility: hidden" <$ guard indeterminate
          , "min-width: auto" <$ guard (not minWidth)
          ]

      barConfig = def
        { _classes = "bar"
        , _style = Dyn $ mkBarStyle
          <$> dIndeterminate <*> dMaybePercent <*> _progressMinWidth
        }

  (progressEl, barEl) <- ui' "div" progressConfig $ do
    (barEl, _) <- ui' "div" barConfig $ do
      for_ _progressBar $ \pContent -> divClass "progress" $ case pContent of
        PercentageBar -> dynText $ maybe mempty percentText <$> dMaybePercent
        FractionDoneBar -> do
          dynText $ tshow <$> dValue
          text $ " / "
          dynText $ tshow . rangeMax <$> dRange
        Bar f -> f

    for_ _progressLabel $ divClass "label"

    pure barEl

  pure $ Progress dMaybePercent progressEl barEl

