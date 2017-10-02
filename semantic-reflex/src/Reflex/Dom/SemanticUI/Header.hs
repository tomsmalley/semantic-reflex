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

data HeaderConfig t = HeaderConfig
  { _subHeader    :: Active t (Maybe Text)

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

  , _attributes   :: Active t (Map Text Text)

  , _component    :: Bool -- This is the "ui" (component) class
  , _item         :: Bool
  , _header       :: HeaderType
  }

instance Default (HeaderConfig t) where
  def = HeaderConfig
    { _subHeader = Static Nothing

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

    , _attributes = Static mempty

    , _component = True
    , _item = False
    , _header = PageHeader
    }

headerConfigClasses :: Reflex t => HeaderConfig t -> Active t ClassText
headerConfigClasses HeaderConfig {..} = mconcat
  [ memptyUnless "icon" <$> _iconHeader
  , memptyUnless "dividing" <$> _dividing
  , memptyUnless "sub" <$> _sub
  , memptyUnless "disabled" <$> _disabled
  , memptyUnless "block" <$> _block
  , memptyUnless "inverted" <$> _inverted
  , memptyUnless "attached" <$> _attached -- FIXME requires two settings

  , toClassText <$> _floated
  , toClassText <$> _aligned
  , toClassText <$> _color
  , toClassText <$> _attachedSide -- FIXME requires two settings

  , memptyUnless "ui" _component
  , memptyUnless "item" _item
  ]

data HeaderType = PageHeader | ContentHeader

data HeaderSize = H1 | H2 | H3 | H4 | H5 deriving (Eq, Show)

headerSizeEl :: HeaderSize -> Text
headerSizeEl H1 = "h1"
headerSizeEl H2 = "h2"
headerSizeEl H3 = "h3"
headerSizeEl H4 = "h4"
headerSizeEl H5 = "h5"

headerSize :: HeaderSize -> ClassText
headerSize H1 = "huge"
headerSize H2 = "large"
headerSize H3 = "medium"
headerSize H4 = "small"
headerSize H5 = "tiny"

data Paragraph m a = Paragraph
  { _content :: m a
  }

instance ToPart (Paragraph m a) where
  toPart = id

instance m ~ m' => UI t m' (Paragraph m a) where
  type Return t m' (Paragraph m a) = a
  ui' (Paragraph content) = el' "p" content

-- | Create a header.
--
-- https://semantic-ui.com/elements/header.html
data Header t m a = Header
  { _size :: HeaderSize
  , _content :: m a
  , _config :: HeaderConfig t
  }

instance ToItem (Header t m a) where
  toItem (Header size content config) = Header size content $
    config { _item = True, _component = False }

instance ToPart (Header t m a) where
  toPart (Header size content config) = Header size content $
    config { _component = False }

type Href = Text
data Anchor m a = Anchor Href (m a)
instance m ~ m' => UI t m' (Anchor m a) where
  type Return t m' (Anchor m a) = (Event t (), a)
  ui' (Anchor href inner) = do
    (a, b) <- elAttr' "a" ("href" =: href <> "class" =: "ui anchor") inner
    return (a, (domEvent Click a, b))

instance (m ~ m', t ~ t') => UI t' m' (Header t m a) where
  type Return t' m' (Header t m a) = a
  ui' (Header size widget config@HeaderConfig {..}) = case _header of
    PageHeader -> elActiveAttr' (headerSizeEl size) attrs content
      where
        attrs = mkAttrs <$> classes <*> _attributes
        mkAttrs c a = "class" =: getClass c <> a
    ContentHeader -> elActiveAttr' "div" attrs content
      where
        attrs = mkAttrs <$> classes <*> _attributes
        mkAttrs c a = "class" =: getClass (c <> headerSize size) <> a
    where
      classes = mconcat ["header", headerConfigClasses config]
      content = do
        runRenderWhen ui' _image
        runRenderWhen ui' _icon
        divClass "content" $ do
          a <- widget
          activeMaybe (divClass "sub header" . text) _subHeader
          return a
