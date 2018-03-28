{-# LANGUAGE TemplateHaskell #-}

-- | Semantic UI rails.
-- https://semantic-ui.com/elements/rail.html
module Reflex.Dom.SemanticUI.Rail
  (

  -- * Rail
    rail, rail'
  , RailConfig (..)
  , RailSide (..)
  , railDividing
  , railInternal
  , railAttached
  , railClose
  , railSize
  , railElConfig

  ) where

import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
import Data.Default
import Data.Semigroup ((<>))
import Reflex
import Reflex.Dom.Core

import Reflex.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data RailSide = LeftRail | RightRail
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToClassText RailSide where
  toClassText LeftRail = "left"
  toClassText RightRail = "right"

data RailClose = Close | VeryClose
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToClassText RailClose where
  toClassText Close = "close"
  toClassText VeryClose = "very close"

data RailConfig t = RailConfig
  { _railDividing :: Active t Bool
  , _railInternal :: Active t Bool
  , _railAttached :: Active t Bool
  , _railClose :: Active t (Maybe RailClose)
  , _railSize :: Active t (Maybe Size)
  , _railElConfig :: ActiveElConfig t
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''RailConfig

instance HasElConfig t (RailConfig t) where
  elConfig = railElConfig

instance Reflex t => Default (RailConfig t) where
  def = RailConfig
    { _railDividing = pure False
    , _railInternal = pure False
    , _railAttached = pure False
    , _railClose = pure Nothing
    , _railSize = pure Nothing
    , _railElConfig = def
    }

-- | Make the rail div classes from the configuration
railConfigClasses :: Reflex t => RailConfig t -> Active t Classes
railConfigClasses RailConfig {..} = dynClasses
  [ pure $ Just "ui rail"
  , boolClass "dividing" _railDividing
  , boolClass "internal" _railInternal
  , boolClass "attached" _railAttached
  , fmap toClassText <$> _railClose
  , fmap toClassText <$> _railSize
  ]

-- | Rail UI Element.
rail'
  :: UI t m => RailSide -> RailConfig t -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
rail' railSide config@RailConfig {..} content
  = ui' "div" elConf content
  where
    elConf = _railElConfig <> def
      { _classes = addClass (toClassText railSide)
               <$> railConfigClasses config }

-- | Rail UI Element.
rail :: UI t m => RailSide -> RailConfig t -> m a -> m a
rail railSide config = fmap snd . rail' railSide config

