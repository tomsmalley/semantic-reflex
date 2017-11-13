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
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE RecursiveDo              #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}

module Reflex.Dom.SemanticUI.Label where

import Data.Default
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core hiding (fromJSString, divClass)

import Reflex.Dom.Active
import Reflex.Dom.SemanticUI.Common

import Reflex.Dom.SemanticUI.Transition

data Ribbon
  = LeftRibbon
  | RightRibbon
  deriving (Eq, Show)

instance ToClassText Ribbon where
  toClassText LeftRibbon = "ribbon"
  toClassText RightRibbon = "right ribbon"

data TopCorner
  = LeftCorner
  | RightCorner
  deriving (Eq, Show)

instance ToClassText TopCorner where
  toClassText LeftCorner = "left corner"
  toClassText RightCorner = "right corner"


-- Icon, Image, Detail

data Label t m a = Label
  { _config :: LabelConfig t
  , _content :: UI Label m a
  }

-- | If a label is attached, it *must* be vertically attached in some way. There
-- can't be a soley horizontally attached label.
data LabelAttached = LabelAttached
  { _vertically :: VerticalAttached
  , _horizontally :: Maybe HorizontalAttached
  }

instance Default LabelAttached where
  def = LabelAttached TopAttached Nothing

-- The selectors are very specific for these cases, it must go
--    "[vertical] [horizontal] attached"
instance ToClassText LabelAttached where
  toClassText (LabelAttached v mh)
    = T.unwords $ catMaybes [ vClass v, hClass <$> mh, Just "attached" ]
    where
      vClass TopAttached = Just "top"
      vClass Attached = Nothing
      vClass BottomAttached = Just "bottom"
      hClass LeftAttached = "left"
      hClass RightAttached = "right"

data LabelConfig t = LabelConfig
  { _attached :: Active t (Maybe LabelAttached)
  , _color :: Active t (Maybe Color)
  , _pointing :: Active t (Maybe Pointing)
  , _ribbon :: Active t (Maybe Ribbon)
  , _corner :: Active t (Maybe TopCorner)
  , _image :: Active t Bool
  , _hidden :: Active t Bool
  , _basic :: Active t Bool
  , _tag :: Active t Bool
  , _floating :: Active t Bool
  , _horizontal :: Active t Bool
  , _link :: Bool
  , _config :: ActiveElConfig t
  }

instance Reflex t => Default (LabelConfig t) where
  def = LabelConfig
    { _attached = pure Nothing
    , _color = pure Nothing
    , _pointing = pure Nothing
    , _ribbon = pure Nothing
    , _corner = pure Nothing
    , _image = pure False
    , _hidden = pure False
    , _basic = pure False
    , _tag = pure False
    , _floating = pure False
    , _horizontal = pure False
    , _link = False
    , _config = def
    }

labelConfigClasses :: Reflex t => LabelConfig t -> Active t Classes
labelConfigClasses LabelConfig {..} = activeClasses
  [ Static $ Just "ui label"
  , fmap toClassText <$> _attached
  , fmap toClassText <$> _color
  , fmap toClassText <$> _pointing
  , fmap toClassText <$> _ribbon
  , fmap toClassText <$> _corner
  , boolClass "hidden" _hidden
  , boolClass "basic" _basic
  , boolClass "tag" _tag
  , boolClass "floating" _floating
  , boolClass "horizontal" _horizontal
  , boolClass "image" _image
  ]


data Pointing = LeftPointing | RightPointing | AbovePointing | BelowPointing
  deriving (Eq, Show)

instance ToClassText Pointing where
  toClassText LeftPointing = "left pointing"
  toClassText RightPointing = "right pointing"
  toClassText AbovePointing = "pointing"
  toClassText BelowPointing = "pointing below" -- Must be in this order

data Detail t = Detail (Active t Text) -- Text or Link
