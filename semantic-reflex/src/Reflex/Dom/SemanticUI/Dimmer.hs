{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Semantic UI dimmers. Pure reflex implementation is provided.
-- https://semantic-ui.com/modules/dimmers.html
module Reflex.Dom.SemanticUI.Dimmer
  (

    dimmer, dimmer'
  , DimmerConfig (..)
  , dimmerConfig_inverted
  , dimmerConfig_page
  , dimmerConfig_dimmed
  , dimmerConfig_transitionType
  , dimmerConfig_duration
  , dimmerConfig_closeOnClick
  , dimmerConfig_elConfig

  ) where

import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)

import Control.Lens ((^.))
import Control.Monad ((<=<))
import Data.Default
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Reflex
import Reflex.Dom.Core

import Data.Time (NominalDiffTime)

import Reflex.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data DimmerConfig t = DimmerConfig
  { _dimmerConfig_inverted :: Dynamic t Bool
  -- ^ Dimmers can be inverted
  , _dimmerConfig_page :: Bool
  -- ^ Dimmers can dim the whole page
  , _dimmerConfig_dimmed :: SetValue' t Direction (Maybe Direction)
  -- ^ Dimmer state control
  , _dimmerConfig_transitionType :: Dynamic t TransitionType
  -- ^ Type of transition to use
  , _dimmerConfig_duration :: Dynamic t NominalDiffTime
  -- ^ Duration of transition
  , _dimmerConfig_closeOnClick :: Dynamic t Bool
  -- ^ User can click out of a dimmer
  , _dimmerConfig_elConfig :: ActiveElConfig t
  -- ^ Config
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''DimmerConfig

instance HasElConfig t (DimmerConfig t) where
  elConfig = dimmerConfig_elConfig

instance Reflex t => Default (DimmerConfig t) where
  def = DimmerConfig
    { _dimmerConfig_inverted = pure False
    , _dimmerConfig_page = False
    , _dimmerConfig_dimmed = SetValue Out Nothing
    , _dimmerConfig_transitionType = pure Fade
    , _dimmerConfig_duration = pure 0.5
    , _dimmerConfig_closeOnClick = pure True
    , _dimmerConfig_elConfig = def
    }

-- | Make the dimmer div classes from the configuration
dimmerConfigClasses :: Reflex t => DimmerConfig t -> Dynamic t Classes
dimmerConfigClasses DimmerConfig {..} = dynClasses'
  [ pure $ Just "ui active dimmer"
  , boolClass "inverted" _dimmerConfig_inverted
  , pure $ if _dimmerConfig_page then Just "page" else Nothing
  ]

-- | Dimmer UI Element.
dimmer'
  :: forall t m a. UI t m => DimmerConfig t -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
dimmer' config@DimmerConfig {..} content = mdo

  let click = gate (current _dimmerConfig_closeOnClick) $ domEvent Click e
      f Nothing d = flipDirection d
      f (Just d) _ = d

  dDir <- holdUniqDyn <=< foldDyn f (_dimmerConfig_dimmed ^. initial) $ leftmost
        [ fromMaybe never $ _dimmerConfig_dimmed ^. event
        , Just Out <$ click ]

  (e, a) <- ui' "div" (mkElConfig $ updated dDir) content

  pure (e, a)

  where

    mkElConfig eDir = _dimmerConfig_elConfig <> def
      { _classes = Dyn $ dimmerConfigClasses config
      , _action = Just $ def
        { _action_initialDirection = _dimmerConfig_dimmed ^. initial
        , _action_transition =
          let f dur dir = Transition Fade (Just dir) $ def
                { _transitionConfig_cancelling = True
                , _transitionConfig_duration = dur
                }
          in attachWith f (current _dimmerConfig_duration) eDir
        }
      }

-- | Dimmer UI Element.
dimmer :: UI t m => DimmerConfig t -> m a -> m a
dimmer c = fmap snd . dimmer' c
