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
import qualified Data.Map as M
import           Data.Maybe (catMaybes)
import           Data.Semigroup ((<>))
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (sequenceA)
import           Reflex
import           Reflex.Dom.Core hiding
  ( checkbox, Checkbox (..), checkbox_value, checkbox_change
  , CheckboxConfig (..), checkboxConfig_attributes, checkboxConfig_setValue
  )

import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data ImageShape = Square | Rounded | Circular deriving (Eq, Show)

instance ToClassText ImageShape where
  toClassText Rounded = "rounded"
  toClassText Circular = "circular"
  toClassText Square = ""

data ImageConfig t = ImageConfig
  { _size :: Active t (Maybe Size)
  , _shape :: Active t ImageShape
  , _avatar :: Active t Bool
  , _floated :: Active t (Maybe Floated)
  , _component :: Bool
  , _title :: Active t (Maybe Text)
  , _hidden :: Active t Bool
  , _config :: ActiveElConfig t
  }

instance Reflex t => Default (ImageConfig t) where
  def = ImageConfig
    { _size = Static Nothing
    , _shape = Static Square
    , _avatar = Static False
    , _floated = Static Nothing
    , _component = False
    , _title = Static Nothing
    , _hidden = Static False
    , _config = def
    }

instance ToPart (Image t) where
  toPart (Image src config) = Image src $ config { _component = True }

imageConfigClasses :: Reflex t => ImageConfig t -> Active t Classes
imageConfigClasses ImageConfig {..} = activeClasses
  [ Static $ justWhen (not _component) "ui image"
  , fmap toClassText <$> _size
  , Just . toClassText <$> _shape
--  , memptyUnless "avatar" <$> _avatar
  , fmap toClassText <$> _floated
  , boolClass "hidden" _hidden
  ]

data Image t = Image
  { _src :: Active t Text
  , _config :: ImageConfig t
  }

instance t ~ t' => UI t' m (Image t) where
  type Return t' m (Image t) = ()
  ui' (Image src config@ImageConfig {..}) = do

    elWithAnim' "img" attrs blank

    where
      attrs = _config <> def
        { _classes = imageConfigClasses config
        , _attrs = mkAttrs <$> src <*> _title
        }
      mkAttrs s t = "src" =: s <> maybe mempty ("title" =:) t

