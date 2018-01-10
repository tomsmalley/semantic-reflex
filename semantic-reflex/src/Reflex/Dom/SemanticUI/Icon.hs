{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module Reflex.Dom.SemanticUI.Icon where

import Control.Lens.TH (makeLenses, makeLensesWith, lensRules, simpleLenses)
import Control.Monad (void)
import Data.Default
import Data.Semigroup ((<>))
import Data.Text (Text)
import Reflex.Dom.Core hiding (fromJSString)

import Reflex.Dom.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data FlagConfig t = FlagConfig
  { _flagElConfig :: ActiveElConfig t
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''FlagConfig

instance Reflex t => Default (FlagConfig t) where
  def = FlagConfig def

data Flipped = HorizontallyFlipped | VerticallyFlipped
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToClassText Flipped where
  toClassText HorizontallyFlipped = "horizontally flipped"
  toClassText VerticallyFlipped = "vertically flipped"

data Rotated = Clockwise | Anticlockwise
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToClassText Rotated where
  toClassText Clockwise = "clockwise rotated"
  toClassText Anticlockwise = "counterclockwise rotated"

data Corner = TopLeft | TopRight | BottomLeft | BottomRight
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToClassText Corner where
  toClassText TopLeft = "top left corner"
  toClassText TopRight = "top right corner"
  toClassText BottomLeft = "bottom left corner"
  toClassText BottomRight = "bottom right corner"

data IconConfig t = IconConfig
  { _iconDisabled :: Active t Bool
  , _iconLoading :: Active t Bool
  , _iconFitted :: Active t Bool
  , _iconSize :: Active t (Maybe Size)
  , _iconLink :: Active t Bool
  , _iconFlipped :: Active t (Maybe Flipped)
  , _iconRotated :: Active t (Maybe Rotated)
  , _iconCircular :: Active t Bool
  , _iconBordered :: Active t Bool
  , _iconColor :: Active t (Maybe Color)
  , _iconInverted :: Active t Bool
  , _iconCorner :: Active t (Maybe Corner)
  , _iconTitle :: Active t (Maybe Text)
  , _iconElConfig :: ActiveElConfig t
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''IconConfig

instance HasElConfig t (IconConfig t) where
  elConfig = iconElConfig

instance Reflex t => Default (IconConfig t) where
  def = IconConfig
    { _iconDisabled = pure False
    , _iconLoading = pure False
    , _iconFitted = pure False
    , _iconSize = pure Nothing
    , _iconLink = pure False
    , _iconFlipped = pure Nothing
    , _iconRotated = pure Nothing
    , _iconCircular = pure False
    , _iconBordered = pure False
    , _iconColor = pure Nothing
    , _iconInverted = pure False
    , _iconCorner = pure Nothing
    , _iconTitle = pure Nothing
    , _iconElConfig = def
    }

iconConfigClasses :: Reflex t => IconConfig t -> Active t Classes
iconConfigClasses IconConfig {..} = activeClasses
  [ Static $ Just "icon"
  , boolClass "disabled" _iconDisabled
  , boolClass "loading" _iconLoading
  , boolClass "fitted" _iconFitted
  , fmap toClassText . nothingIf Medium <$> _iconSize
  , boolClass "link" _iconLink
  , fmap toClassText <$> _iconFlipped
  , fmap toClassText <$> _iconRotated
  , boolClass "circular" _iconCircular
  , boolClass "bordered" _iconBordered
  , fmap toClassText <$> _iconColor
  , boolClass "inverted" _iconInverted
  , fmap toClassText <$> _iconCorner
  ]

data IconsConfig t = IconsConfig
  { _iconsSize :: Active t (Maybe Size)
  , _iconsElConfig :: ActiveElConfig t
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''IconsConfig

instance Reflex t => Default (IconsConfig t) where
  def = IconsConfig
    { _iconsSize = pure Nothing
    , _iconsElConfig = def
    }

iconsConfigClasses :: Reflex t => IconsConfig t -> Active t Classes
iconsConfigClasses IconsConfig {..} = activeClasses
  [ Static $ Just "icons"
  , fmap toClassText <$> _iconsSize
  ]


icon' :: MonadWidget t m => Active t Text -> IconConfig t -> m (El t)
icon' activeIcon config@IconConfig {..}
  = fst <$> uiElement' "i" elConf blank
  where
    elConf = _iconElConfig <> def
      { _classes = addClass <$> activeIcon <*> iconConfigClasses config
      , _attrs = maybe mempty ("title" =:) <$> _iconTitle
      }

-- This is for inclusion in other element configs
data Icon t = Icon (Active t Text) (IconConfig t)

icon :: MonadWidget t m => Active t Text -> IconConfig t -> m ()
icon i = void . icon' i

icons' :: MonadWidget t m => IconsConfig t -> m a -> m (El t, a)
icons' config@IconsConfig {..} = uiElement' "i" elConf
  where
    elConf = _iconsElConfig <> def
      { _classes = iconsConfigClasses config }

icons :: MonadWidget t m => IconsConfig t -> m a -> m a
icons config = fmap snd . icons' config


flag' :: MonadWidget t m => Active t Text -> FlagConfig t -> m (El t)
flag' flagActive FlagConfig {..}
  = fst <$> uiElement' "i" config blank
  where
    config = _flagElConfig
      & elConfigClasses .~ (flip addClass "flag" <$> flagActive)

flag :: MonadWidget t m => Active t Text -> FlagConfig t -> m ()
flag f = void . flag' f

