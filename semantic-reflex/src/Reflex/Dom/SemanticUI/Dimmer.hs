{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Semantic UI dimmers. Pure reflex implementation is provided.
-- https://semantic-ui.com/modules/dimmers.html
module Reflex.Dom.SemanticUI.Dimmer
  (

    dimmer, dimmer'
  , DimmerConfig (..)
  , dimmerInverted
  , dimmerPage
  , dimmerDimmed
  , dimmerTransitionType
  , dimmerDuration
  , dimmerCloseOnClick
  , dimmerElConfig

  ) where

import Control.Lens ((^.), (?~))
import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
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
  { _dimmerInverted :: Dynamic t Bool
  -- ^ Dimmers can be inverted
  , _dimmerPage :: Bool
  -- ^ Dimmers can dim the whole page
  , _dimmerDimmed :: SetValue' t Direction (Maybe Direction)
  -- ^ Dimmer state control
  , _dimmerTransitionType :: Dynamic t TransitionType
  -- ^ Type of transition to use
  , _dimmerDuration :: Dynamic t NominalDiffTime
  -- ^ Duration of transition
  , _dimmerCloseOnClick :: Dynamic t Bool
  -- ^ User can click out of a dimmer
  , _dimmerElConfig :: ActiveElConfig t
  -- ^ Config
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''DimmerConfig

instance HasElConfig t (DimmerConfig t) where
  elConfig = dimmerElConfig

instance Reflex t => Default (DimmerConfig t) where
  def = DimmerConfig
    { _dimmerInverted = pure False
    , _dimmerPage = False
    , _dimmerDimmed = SetValue Out Nothing
    , _dimmerTransitionType = pure Fade
    , _dimmerDuration = pure 0.75
    , _dimmerCloseOnClick = pure True
    , _dimmerElConfig = def
    }

-- | Make the dimmer div classes from the configuration
dimmerConfigClasses :: Reflex t => DimmerConfig t -> Dynamic t Classes
dimmerConfigClasses DimmerConfig {..} = dynClasses'
  [ pure $ Just "ui active dimmer"
  , boolClass "inverted" _dimmerInverted
  , pure $ if _dimmerPage then Just "page" else Nothing
  ]

-- | Dimmer UI Element.
dimmer'
  :: forall t m a. UI t m => DimmerConfig t -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
dimmer' config@DimmerConfig {..} content = do
  rec

    let click = ffilter id $ tag (current _dimmerCloseOnClick) $ domEvent Click e
        f Nothing d = flipDirection d
        f (Just d) _ = d

    dDir <- holdUniqDyn <=< foldDyn f (_dimmerDimmed ^. initial) $ leftmost
          [ fromMaybe never $ _dimmerDimmed ^. event
          , Just Out <$ click ]

    (e, a) <- uiElement' "div" (mkElConfig $ updated dDir) content

  pure (e, a)

  where

    mkElConfig eDir = _dimmerElConfig <> def
      { _classes = Dyn $ dimmerConfigClasses config
      , _action = Just $ mkAction eDir
      }

    mkAction eDir = def
      & actionInitialDirection .~ _dimmerDimmed ^. initial
      & actionEvent ?~
        (uncurry mkTransition <$> attachPromptlyDyn _dimmerDuration eDir)

    mkTransition dur dir = Transition Fade $ def
      & transitionCancelling .~ True
      & transitionDirection ?~ dir
      & transitionDuration .~ dur

dimmer :: UI t m => DimmerConfig t -> m a -> m a
dimmer c = fmap snd . dimmer' c

