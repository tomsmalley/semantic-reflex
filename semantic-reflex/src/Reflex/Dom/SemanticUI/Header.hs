{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Reflex.Dom.SemanticUI.Header where

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

import Reflex.Dom.SemanticUI.Icon
import Reflex.Dom.SemanticUI.Image
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data HeaderConfig t m = HeaderConfig
  { _subHeader    :: Maybe (m ())

  , _iconHeader   :: Active t Bool
  , _dividing     :: Active t Bool
  , _sub          :: Active t Bool
  , _disabled     :: Active t Bool
  , _block        :: Active t Bool
  , _inverted     :: Active t Bool
  , _attached     :: Active t Bool

  , _image        :: RenderWhen t (Image t)
  , _icon         :: RenderWhen t (Icon t)

  , _floated      :: Active t (Maybe Floated)
  , _aligned      :: Active t (Maybe Aligned)
  , _color        :: Active t (Maybe Color)
  , _attachedSide :: Active t (Maybe VerticalAttached)

  , _component    :: Bool -- This controls the "ui" class
  , _item         :: Bool
  , _header       :: HeaderType
  , _config :: ActiveElConfig t
  }

instance Default (HeaderConfig t m) where
  def = HeaderConfig
    { _subHeader = Nothing

    , _iconHeader = Static False
    , _dividing = Static False
    , _sub = Static False
    , _disabled = Static False
    , _block = Static False
    , _inverted = Static False
    , _attached = Static False

    , _image = NeverRender
    , _icon = NeverRender

    , _floated = Static Nothing
    , _aligned = Static Nothing
    , _color = Static Nothing
    , _attachedSide = Static Nothing

    , _component = True
    , _item = False
    , _header = PageHeader
    , _config = def
    }

headerConfigClasses :: Reflex t => HeaderConfig t m -> Active t Classes
headerConfigClasses HeaderConfig {..} = activeClasses
  [ Static $ Just "header"
  , boolClass "icon" _iconHeader
  , boolClass "dividing" _dividing
  , boolClass "sub" _sub
  , boolClass "disabled" _disabled
  , boolClass "block" _block
  , boolClass "inverted" _inverted
  , boolClass "attached" _attached -- FIXME requires two settings

  , fmap toClassText <$> _floated
  , fmap toClassText <$> _aligned
  , fmap toClassText <$> _color
  , fmap toClassText <$> _attachedSide -- FIXME requires two settings

  , boolClass "ui" $ Static _component
  , boolClass "item" $ Static _item
  ]

data HeaderType = PageHeader | ContentHeader

data HeaderSize = H1 | H2 | H3 | H4 | H5 deriving (Eq, Show)

headerSizeEl :: HeaderSize -> Text
headerSizeEl H1 = "h1"
headerSizeEl H2 = "h2"
headerSizeEl H3 = "h3"
headerSizeEl H4 = "h4"
headerSizeEl H5 = "h5"

headerSizeText :: HeaderSize -> Text
headerSizeText H1 = "huge"
headerSizeText H2 = "large"
headerSizeText H3 = "medium"
headerSizeText H4 = "small"
headerSizeText H5 = "tiny"

--data Paragraph m a = Paragraph
--  { _content :: m a
--  }
--
--instance ToPart (Paragraph m a) where
--  toPart = id
--
--instance m ~ m' => UI t m' (Paragraph m a) where
--  type Return t m' (Paragraph m a) = a
--  ui' (Paragraph content) = el' "p" content

-- | Create a header.
--
-- https://semantic-ui.com/elements/header.html
data Header t m a = Header
  { _size :: HeaderSize
  , _content :: m a
  , _config :: HeaderConfig t m
  }

instance ToItem (Header t m a) where
  toItem (Header size content config) = Header size content $
    config { _item = True, _component = False }

instance ToPart (Header t m a) where
  toPart (Header size content config) = Header size content $
    config { _component = False }

--type Href = Text
--data Anchor m a = Anchor Href (m a)
--instance m ~ m' => UI t m' (Anchor m a) where
--  type Return t m' (Anchor m a) = (Event t (), a)
--  ui' (Anchor href inner) = do
--    (a, b) <- elAttr' "a" ("href" =: href <> "class" =: "ui anchor") inner
--    return (a, (domEvent Click a, b))

instance (m ~ m', t ~ t') => UI t' m' (Header t m a) where
  type Return t' m' (Header t m a) = a
  ui' (Header size widget config@HeaderConfig {..}) = case _header of
    PageHeader -> elWithAnim' (headerSizeEl size) attrs content
    ContentHeader -> elWithAnim' "div" attrs' content
      where
        attrs' = attrs { _classes = addClass (headerSizeText size) <$> classes }
    where
      attrs = _config <> def
        { _classes = classes
        }
      classes = headerConfigClasses config
      content = do
        runRenderWhen ui' _image
        runRenderWhen ui' _icon
        divClass "content" $ do
          a <- widget
          maybe blank (divClass "sub header") _subHeader
          return a
