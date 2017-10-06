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

import           Data.Default
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import           Reflex.Dom.Core hiding (fromJSString)

import           Reflex.Dom.SemanticUI.Common

import Reflex.Dom.SemanticUI.Icon
import Reflex.Dom.SemanticUI.Image
import Reflex.Dom.SemanticUI.Transition

data Ribbon
  = LeftRibbon
  | RightRibbon
  deriving (Eq, Show)

instance ToClassText Ribbon where
  toClassText LeftRibbon = "ribbon"
  toClassText RightRibbon = "right ribbon"

data Label t = Label
  { _text :: Active t Text
  , _config :: LabelConfig t
  }

data LabelConfig t = LabelConfig
  { _vAttached :: Active t (Maybe VerticalAttached)
  , _hAttached :: Active t (Maybe HorizontalAttached)
  , _color :: Active t (Maybe Color)
  , _pointing :: Active t (Maybe Pointing)
  , _ribbon :: Active t (Maybe Ribbon)
  , _leftIcon :: RenderWhen t (Icon t)
  , _rightIcon :: RenderWhen t (Icon t)
  , _image :: RenderWhen t (Image t)
  , _imageFocus :: Active t Bool
  , _hidden :: Active t Bool
  , _basic :: Active t Bool
  , _detail :: Active t (Maybe Text)
  , _link :: Bool
  , _config :: ActiveElConfig t
  }

instance Reflex t => Default (LabelConfig t) where
  def = LabelConfig
    { _vAttached = pure Nothing
    , _hAttached = pure Nothing
    , _color = pure Nothing
    , _pointing = pure Nothing
    , _ribbon = pure Nothing
    , _leftIcon = NeverRender
    , _rightIcon = NeverRender
    , _image = NeverRender
    , _imageFocus = pure True
    , _hidden = pure False
    , _basic = pure False
    , _detail = pure Nothing
    , _link = False
    , _config = def
    }

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

labelConfigClasses :: Reflex t => LabelConfig t -> Active t Classes
labelConfigClasses LabelConfig {..} = activeClasses
  [ Static $ Just "ui label"
  , combineAttached <$> _vAttached <*> _hAttached
  , fmap toClassText <$> _color
  , fmap toClassText <$> _pointing
  , fmap toClassText <$> _ribbon
  , boolClass "hidden" _hidden
  , boolClass "basic" _basic
  , boolClass "image" _imageFocus
  ]

data Pointing = LeftPointing | RightPointing | AbovePointing | BelowPointing
  deriving (Eq, Show)

instance ToClassText Pointing where
  toClassText LeftPointing = "left pointing"
  toClassText RightPointing = "right pointing"
  toClassText AbovePointing = "above pointing"
  toClassText BelowPointing = "below pointing"

data LabelResult t m = LabelResult
  { _leftIcon :: Dynamic t (Maybe (El t, Return t m (Icon t)))
  , _rightIcon :: Dynamic t (Maybe (El t, Return t m (Icon t)))
  , _image :: Dynamic t (Maybe (El t, Return t m (Image t)))
  }

instance (t ~ t') => UI t' m (Label t) where
  type Return t' m (Label t) = LabelResult t m

  ui' (Label txt config@LabelConfig {..}) = do
    (e, (imageResult, leftIconResult, rightIconResult)) <-
      elWithAnim' elType attrs $ do
        imageResult <- runRenderWhen part' _image
        leftIconResult <- runRenderWhen ui' _leftIcon
        activeText txt
        rightIconResult <- runRenderWhen ui' _rightIcon
        activeMaybe (divClass "detail" . text) _detail
        return (imageResult, leftIconResult, rightIconResult)
    return $ (e, LabelResult
      { _leftIcon = leftIconResult
      , _rightIcon = rightIconResult
      , _image = imageResult
      })
    where
      attrs = _config <> def
        { _classes = labelConfigClasses config
        }
      elType = if _link then "a" else "div"
