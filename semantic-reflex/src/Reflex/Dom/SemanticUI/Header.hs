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

data HeaderConfig t m = HeaderConfig
  { _image :: Maybe (Image t)
  , _icon :: Maybe (Icon t)
  , _subHeader :: Maybe (m ())
  , _header :: HeaderType
  , _dividing :: Bool
  , _floated :: Maybe Floated
  , _item :: Bool
  , _component :: Bool -- This is the "ui" (component) class
  , _attributes :: Map Text Text
  }

instance Default (HeaderConfig t m) where
  def = HeaderConfig
    { _image = Nothing
    , _icon = Nothing
    , _subHeader = Nothing
    , _header = PageHeader
    , _dividing = False
    , _floated = Nothing
    , _item = False
    , _component = True
    , _attributes = mempty
    }

headerConfigClasses :: HeaderConfig t m -> ClassText
headerConfigClasses HeaderConfig {..} = mconcat
  [ memptyUnless "dividing" _dividing
  , memptyUnless "item" _item
  , memptyUnless "ui" _component
  , toClassText _floated
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

data Paragraph = Paragraph
  { _text :: Text
  }

instance ToPart Paragraph where
  toPart = id

instance UI t m Paragraph where
  type Return t m Paragraph = ()
  ui' (Paragraph txt) = el' "p" $ text txt

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

type Href = Text
data Anchor m a = Anchor Href (m a)
instance m ~ m' => UI t m' (Anchor m a) where
  type Return t m' (Anchor m a) = (Event t (), a)
  ui' (Anchor href inner) = do
    (a, b) <- elAttr' "a" ("href" =: href <> "class" =: "ui anchor") inner
    return (a, (domEvent Click a, b))

instance (t ~ t', m ~ m') => UI t' m' (Header t m a) where
  type Return t' m' (Header t m a) = a
  ui' (Header size widget config@HeaderConfig {..}) = case _header of
    PageHeader -> elAttr' (headerSizeEl size) attrs iContent
      where attrs = "class" =: getClass classes <> _attributes
    ContentHeader -> elAttr' "div" attrs iContent
      where attrs = "class" =: getClass (headerSize size <> classes) <> _attributes
    where
      classes = mconcat ["header", headerConfigClasses config]
      iContent
        | Just img <- _image = ui img >> content
        | Just icon <- _icon = ui icon >> content
        | otherwise = content
      content
        | Just sub <- _subHeader = divClass "content" $ do
            a <- widget
            divClass "sub header" sub
            return a
        | otherwise = divClass "content" widget
