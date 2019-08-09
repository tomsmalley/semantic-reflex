{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Semantic UI sidebars.
-- https://semantic-ui.com/modules/sidebar.html
module Reflex.Dom.SemanticUI.Sidebar
  (

    sidebar--, sidebar'
  , SidebarConfig (..)
  , sidebarConfig_dimming
  , sidebarConfig_closeOnClick
  , sidebarConfig_transition
  , sidebarConfig_width
  , Side (..)
  , SidebarTransition (..)
  , SidebarWidth (..)

  ) where

import Control.Lens ((?~), (<>~))
import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
import Data.Default
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Reflex
import Reflex.Dom.Core

import Reflex.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data Side
  = Side_Top
  | Side_Bottom
  | Side_Left
  | Side_Right
  deriving (Eq, Ord, Enum, Bounded, Read, Show)

instance ToClassText Side where
  toClassText = \case
    Side_Top -> "top"
    Side_Bottom -> "bottom"
    Side_Left -> "left"
    Side_Right -> "right"

data SidebarWidth
  = SidebarWidth_VeryThin
  | SidebarWidth_Thin
  | SidebarWidth_Medium
  | SidebarWidth_Wide
  | SidebarWidth_VeryWide
  deriving (Eq, Ord, Enum, Bounded, Read, Show)

instance ToClassText SidebarWidth where
  toClassText = \case
    SidebarWidth_VeryThin -> "very thin"
    SidebarWidth_Thin -> "thin"
    SidebarWidth_Medium -> ""
    SidebarWidth_Wide -> "wide"
    SidebarWidth_VeryWide -> "very wide"

data SidebarTransition
  = SidebarTransition_Overlay
  | SidebarTransition_Push
  | SidebarTransition_ScaleDown
  | SidebarTransition_Uncover
  | SidebarTransition_SlideAlong
  | SidebarTransition_SlideOut
  deriving (Eq, Ord, Enum, Bounded, Read, Show)

instance ToClassText SidebarTransition where
  toClassText = \case
    SidebarTransition_Overlay -> "overlay"
    SidebarTransition_Push -> "push"
    SidebarTransition_ScaleDown -> "scale down"
    SidebarTransition_Uncover -> "uncover"
    SidebarTransition_SlideAlong -> "slide along"
    SidebarTransition_SlideOut -> "slide out"

data SidebarConfig t = SidebarConfig
  { _sidebarConfig_dimming :: Dynamic t Bool
  -- ^ Sidebars can dim the inner content
  , _sidebarConfig_closeOnClick :: Dynamic t Bool
  -- ^ User can click out of a sidebar by clicking on the dimmable space
  , _sidebarConfig_transition :: Dynamic t SidebarTransition
  -- ^ The type of transition to use
  , _sidebarConfig_width :: Dynamic t SidebarWidth
  -- ^ (default: 'SideBarSize_Medium') Sidebars can specify a width
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''SidebarConfig

instance Reflex t => Default (SidebarConfig t) where
  def = SidebarConfig
    { _sidebarConfig_transition = pure SidebarTransition_Overlay
    , _sidebarConfig_dimming = pure False
    , _sidebarConfig_closeOnClick = pure True
    , _sidebarConfig_width = pure SidebarWidth_Medium
    }

sidebarTransitionClasses :: Bool -> Text -> Direction -> TransitionState -> Maybe Classes
sidebarTransitionClasses _ t direction state = Just . Classes $ mconcat
  [ [ t ]
  , case state of
    TransitionState_Animating -> ["animating"]
    TransitionState_Final -> []
  , case direction of
    Out -> []
    In -> ["visible"]
  ]

-- | Make the sidebar div classes from the configuration
sidebarConfigClasses :: Reflex t => Dynamic t Side -> SidebarConfig t -> Dynamic t Classes
sidebarConfigClasses side SidebarConfig {..} = dynClasses'
  [ pure $ Just "ui sidebar"
  , Just . toClassText <$> side
  , Just . toClassText <$> _sidebarConfig_width
  ]

-- | Sidebar controller. This function will orchestrate the necessary config
-- changes to the surrounding elements via configuration transformation
-- functions. You must apply these functions to the top level element config in
-- order for the sidebar to work correctly.
sidebar
  :: forall t m js a b. UI js t m
  => Dynamic t Side -- ^ Which side the sidebar is on
  -> Direction  -- ^ The starting direction
  -> Event t (Maybe Direction) -- ^ Direction updates
  -> SidebarConfig t -- ^ Config
  -> (forall x. (forall cfg. HasElConfig t cfg => cfg -> cfg) -> m x -> m x)
  -- ^ Wrapping element
  -> ((forall cfg. HasElConfig t cfg => cfg -> cfg) -> m a)
  -- ^ Sidebar content
  -> m b
  -- ^ Content the sidebar is accompanying
  -> m (a, b)
sidebar side ini change' config@SidebarConfig {..} wrapper sidebarContent pusherContent = wrapper (& classes <>~ "pushable") $ mdo
  direction <- foldDyn (\m d -> fromMaybe (flipDirection d) m) ini change
  let change = leftmost [change', Just Out <$ gate (current _sidebarConfig_closeOnClick) (domEvent Click e)]
  a <- sidebarContent $ \cfg -> cfg
    & classes <>~ Dyn (sidebarConfigClasses side config)
    & action ?~ def
      { _action_initialDirection = ini
      , _action_transition =
        let f t dir = Transition (CustomTransition $ toClassText t) dir $ def
              { _transitionConfig_cancelling = True
              }
        in attachWith f (current _sidebarConfig_transition) change
      , _action_transitionStateClasses = sidebarTransitionClasses
      }
  let pusherClasses dimming = Classes . ("pusher" :) . \case
        In -> if dimming then ["dimmed"] else []
        Out -> []
  (e, b) <- ui' "div" (def & classes .~ Dyn (pusherClasses <$> _sidebarConfig_dimming <*> direction)) $ do
    pusherContent
  pure (a, b)
