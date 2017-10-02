{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Reflex.Dom.SemanticUI.Image where

import           Data.Default (Default (def))
import Data.Map (Map)
import           Data.Maybe (catMaybes)
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Reflex
import           Reflex.Dom.Core hiding
  ( checkbox, Checkbox (..), checkbox_value, checkbox_change
  , CheckboxConfig (..), checkboxConfig_attributes, checkboxConfig_setValue
  )

import Reflex.Dom.SemanticUI.Common

data ImageRounded = Rounded | Circular deriving (Eq, Show)

instance ToClassText ImageRounded where
  toClassText Rounded = "rounded"
  toClassText Circular = "circular"

data ImageConfig t = ImageConfig
  { _size :: Dynamic t (Maybe Size)
  , _rounded :: Dynamic t (Maybe ImageRounded)
  , _avatar :: Dynamic t Bool
  , _floated :: Dynamic t (Maybe Floated)
  , _hidden :: Dynamic t Bool
  , _component :: Bool
  }

instance Reflex t => Default (ImageConfig t) where
  def = ImageConfig
    { _size = pure Nothing
    , _rounded = pure Nothing
    , _avatar = pure False
    , _floated = pure Nothing
    , _hidden = pure False
    , _component = False
    }

instance ToPart (Image t) where
  toPart (Image src config) = Image src $ config { _component = True }

imageConfigClasses :: Reflex t => ImageConfig t -> Dynamic t ClassText
imageConfigClasses ImageConfig {..} = mconcat
  [ toClassText <$> _size
  , toClassText <$> _rounded
  , memptyUnless "avatar" <$> _avatar
  , toClassText <$> _floated
  , memptyUnless "hidden" <$> _hidden
  ]

data Image t = Image
  { _src :: Dynamic t Text
  , _config :: ImageConfig t
  }

instance t ~ t' => UI t' m (Image t) where
  type Return t' m (Image t) = ()
  ui' (Image src config@ImageConfig {..})
    = elDynAttr' "img" (mkAttrs <$> src <*> imageConfigClasses config) blank
    where
      mkAttrs s c = "src" =: s <> "class" =:
        getClass (if _component then  c else mconcat ["ui", "image", c])

