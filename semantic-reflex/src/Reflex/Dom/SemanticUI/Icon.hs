{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DuplicateRecordFields                      #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE OverloadedLists          #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE RecursiveDo              #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE UndecidableInstances     #-}

module Reflex.Dom.SemanticUI.Icon where

import Data.Default
import Data.Text (Text)
import Reflex.Dom.Core hiding (fromJSString)

import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data Flag t = Flag (Active t Text) (FlagConfig t)

data FlagConfig t = FlagConfig
  { _config :: ActiveElConfig t
  }

instance Default (FlagConfig t) where
  def = FlagConfig def

data Icon t = Icon (Active t Text) (IconConfig t)

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
  { _disabled :: Active t Bool
  , _loading :: Active t Bool
  , _fitted :: Active t Bool
  , _size :: Active t (Maybe Size)
  , _link :: Active t Bool
  , _flipped :: Active t (Maybe Flipped)
  , _rotated :: Active t (Maybe Rotated)
  , _circular :: Active t Bool
  , _bordered :: Active t Bool
  , _color :: Active t (Maybe Color)
  , _inverted :: Active t Bool
  , _corner :: Active t (Maybe Corner)
  , _title :: Active t (Maybe Text)
  , _config :: ActiveElConfig t
  }

instance Reflex t => Default (IconConfig t) where
  def = IconConfig
    { _disabled = pure False
    , _loading = pure False
    , _fitted = pure False
    , _size = pure Nothing
    , _link = pure False
    , _flipped = pure Nothing
    , _rotated = pure Nothing
    , _circular = pure False
    , _bordered = pure False
    , _color = pure Nothing
    , _inverted = pure False
    , _corner = pure Nothing
    , _title = pure Nothing
    , _config = def
    }

iconConfigClasses :: Reflex t => IconConfig t -> Active t Classes
iconConfigClasses IconConfig {..} = activeClasses
  [ Static $ Just "icon"
  , boolClass "disabled" _disabled
  , boolClass "loading" _loading
  , boolClass "fitted" _fitted
  , fmap toClassText . nothingIf Medium <$> _size
  , boolClass "link" _link
  , fmap toClassText <$> _flipped
  , fmap toClassText <$> _rotated
  , boolClass "circular" _circular
  , boolClass "bordered" _bordered
  , fmap toClassText <$> _color
  , boolClass "inverted" _inverted
  , fmap toClassText <$> _corner
  ]

data Icons t m a = Icons (IconsConfig t) (Restrict Icons m a)

data IconsConfig t = IconsConfig
  { _size :: Active t (Maybe Size)
  , _config :: ActiveElConfig t
  }

instance Default (IconsConfig t) where
  def = IconsConfig
    { _size = Static Nothing
    , _config = def
    }

iconsConfigClasss :: Reflex t => IconsConfig t -> Active t Classes
iconsConfigClasss IconsConfig {..} = activeClasses
  [ Static $ Just "icons"
  , fmap toClassText <$> _size
  ]

