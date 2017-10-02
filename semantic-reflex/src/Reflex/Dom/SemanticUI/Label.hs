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

data Ribbon
  = LeftRibbon
  | RightRibbon
  deriving (Eq, Show)

instance ToClassText Ribbon where
  toClassText LeftRibbon = "ribbon"
  toClassText RightRibbon = "right ribbon"

data Label t = Label
  { _text :: Dynamic t Text
  , _config :: LabelConfig t
  }

data LabelConfig t = LabelConfig
  { _vAttached :: Dynamic t (Maybe VerticalAttached)
  , _hAttached :: Dynamic t (Maybe HorizontalAttached)
  , _color :: Dynamic t (Maybe Color)
  , _pointing :: Dynamic t (Maybe Pointing)
  , _ribbon :: Dynamic t (Maybe Ribbon)
  , _leftIcon :: RenderWhen t (Icon t)
  , _rightIcon :: RenderWhen t (Icon t)
  , _image :: RenderWhen t (Image t)
  , _imageFocus :: Dynamic t Bool
  , _hidden :: Dynamic t Bool
  , _basic :: Dynamic t Bool
  , _detail :: Dynamic t (Maybe Text)
  , _link :: Bool
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
    }

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

labelConfigClasses :: Reflex t => LabelConfig t -> Dynamic t ClassText
labelConfigClasses LabelConfig {..} = mconcat
  [ combineAttached <$> _vAttached <*> _hAttached
  , toClassText <$> _color
  , toClassText <$> _pointing
  , toClassText <$> _ribbon
  , memptyUnless "hidden" <$> _hidden
  , memptyUnless "basic" <$> _basic
  , memptyUnless "image" <$> _imageFocus
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
      elDynAttr' elType (fmap attrs $ labelConfigClasses config) $ do
        imageResult <- runRenderWhen part' _image
        leftIconResult <- runRenderWhen ui' _leftIcon
        dynText txt
        rightIconResult <- runRenderWhen ui' _rightIcon
        _ <- dyn $ maybe blank (divClass "detail" . text) <$> _detail
        return (imageResult, leftIconResult, rightIconResult)
    return $ (e, LabelResult
      { _leftIcon = leftIconResult
      , _rightIcon = rightIconResult
      , _image = imageResult
      })
    where
      attrs classes = "class" =: getClass ("ui" <> "label" <> classes)
      elType = if _link then "a" else "div"
