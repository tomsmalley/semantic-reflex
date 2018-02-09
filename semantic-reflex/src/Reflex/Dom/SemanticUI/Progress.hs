{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Semantic UI progress module
module Reflex.Dom.SemanticUI.Progress where

import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
import Data.Default
import Data.Foldable (for_)
import Data.Maybe (catMaybes)
import Data.Semigroup ((<>))
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

-- | Configuration of a progress bar widget.
data ProgressConfig t m = ProgressConfig
  { _progressBar :: Maybe (Bar m)
  -- ^ (default: 'Nothing') Content inside the bar
  , _progressLabel :: Maybe (m ())
  -- ^ (default: 'Nothing') Widget below the bar
  , _progressIndicating :: Dynamic t Bool
  -- ^ (default: 'False') If enabled, changes the bar colour according to the percentage
  , _progressActive :: Dynamic t Bool
  -- ^ (default: 'False') If enabled, show a pulsing animation on the bar
  , _progressBatchUpdates :: Bool
  -- ^ (default: 'True') If enabled, updates which occur faster than '_progressDuration' are
  -- batched together and applied simultaneously. This is to allow smooth
  -- animations, but the bar state will then be behind the actual state by
  -- time equal to '_progressDuration' (in cases where batching is triggered).
  , _progressDuration :: NominalDiffTime
  -- ^ (default: '0.3') Duration of bar animation in seconds
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
    , _progressBatchUpdates = True
    , _progressDuration = 0.3
    , _progressElConfig = def
    }

-- | Make the progress div classes from the configuration
progressConfigClasses :: Reflex t => Dynamic t Int -> ProgressConfig t m -> Dynamic t Classes
progressConfigClasses dPercent ProgressConfig {..} = dynClasses'
  [ pure $ Just "ui progress"
  , boolClass "indicating" _progressIndicating
  , boolClass "active" _progressActive
  , ffor dPercent $ \p -> if p == 100 then Just "success" else Nothing
  ]

-- | Result of running a progress widget.
data Progress t = Progress
  { _progressValue :: Dynamic t Int
  -- ^ The current value (can range from min to max)
  , _progressPercent :: Dynamic t Int
  -- ^ The current percentage (can range from 0 to 100)
  }

-- | Display a progress widget, given minimum and maximum values, the initial
-- value, and an 'Event' to update the current value.
progress
  :: UI t m
  => (Int, Int) -> Int -> Event t (Int -> Int) -> ProgressConfig t m -> m (Progress t)
progress mm i evt = fmap snd . progress' mm i evt

-- | Display a progress widget, given minimum and maximum values, the initial
-- value, and an 'Event' to update the current value. Also returns the
-- "progress" div widget.
progress'
  :: UI t m => (Int, Int) -> Int -> Event t (Int -> Int) -> ProgressConfig t m
  -> m (Element EventResult (DomBuilderSpace m) t, (Progress t))
progress' (vMin, vMax) initialValue eUpdate config@ProgressConfig{..} = do

  let clamp = min vMax . max vMin

  -- Batch updates to smooth animations
  eUpdateBatch <- case _progressBatchUpdates of
    True -> fmap (foldr (flip (.)) id) <$> batchOccurrencesImmediate _progressDuration eUpdate
    False -> pure eUpdate

  -- Apply the functions to the initial value, clamping the results
  dValue <- holdUniqDyn =<< foldDyn (\f x -> clamp $ f x) (clamp initialValue) eUpdateBatch

  let vDiff = vMax - vMin
      -- Feature scale the value to a percent scale (0-100)
      dPercent = ffor dValue $ \v -> 100 * (v - vMin) `div` vDiff

  let progressConfig = _progressElConfig <> def
        { _classes = Dyn $ progressConfigClasses dPercent config
        , _attrs = Dyn $ (\p -> "data-percent" =: tshow p) <$> dPercent
        }

  let barConfig = def
        { _classes = "bar"
        , _style = Dyn $ ffor dPercent $ \w -> Style $ T.intercalate ";" $ catMaybes
          [ if _progressBatchUpdates
            then Just $ "transition-duration: " <> tshow _progressDuration
            else Nothing
          , Just $ "width:" <> tshow w <> "%"
          ]
        }

  (progressEl, _) <- uiElement' "div" progressConfig $ do
    uiElement "div" barConfig $ do
      for_ _progressBar $ \pContent -> divClass "progress" $ case pContent of
        PercentageBar -> do
          dynText $ tshow <$> dPercent
          text "%"
        FractionDoneBar -> do
          dynText $ tshow <$> dValue
          text $ " / " <> tshow vMax
        Bar f -> f

    for_ _progressLabel $ divClass "label"

  pure (progressEl, Progress dValue dPercent)

