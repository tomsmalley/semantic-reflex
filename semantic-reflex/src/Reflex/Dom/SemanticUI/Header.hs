{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Reflex.Dom.SemanticUI.Header where

import Control.Lens
import Data.Default (Default (def))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Semigroup ((<>))
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Reflex
import Reflex.Dom.Core hiding
  ( checkbox, Checkbox (..), checkbox_value, checkbox_change
  , CheckboxConfig (..), checkboxConfig_attributes, checkboxConfig_setValue
  )

import Reflex.Dom.SemanticUI.Icon
import Reflex.Dom.SemanticUI.Image
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition hiding (divClass)

data Paragraph t m a = Paragraph (Restrict Inline m a)

instance (t ~ t', m ~ m') => UI t' m' None (Paragraph t m a) where
  type Return t' m' (Paragraph t m a) = a
  ui' (Paragraph contents)
    = reRestrict $ elWithAnim' "p" def $ reRestrict contents

instance (t ~ t', m ~ m') => UI t' m' Inline (Paragraph t m a) where
  ui' = unRestrict . ui'

data PlainText t = Text (Active t T.Text)

instance (t ~ t') => UI t' m None (PlainText t) where
  type Return t' m (PlainText t) = ()
  -- TODO FIXME no span please
  ui' (Text t) = reRestrict $ el' "span" `mapRestrict` activeText t

instance t ~ t' => UI t' m' Inline (PlainText t) where
  ui' = unRestrict . ui'

instance IsString (PlainText t) where
  fromString str = Text $ Static $ fromString str

data Anchor t m a = Anchor (Restrict Inline m a) (AnchorConfig t)
data AnchorConfig t = AnchorConfig
  { _href :: Active t (Maybe Text)
  , _config :: ActiveElConfig t
  }

instance Default (AnchorConfig t) where
  def = AnchorConfig
    { _href = Static Nothing
    , _config = def
    }

data AnchorResult t a = AnchorResult
  { _click :: Event t ()
  , _content :: a
  }

instance (t ~ t', m ~ m') => UI t' m' None (Anchor t m a) where
  type Return t' m' (Anchor t m a) = AnchorResult t a
  ui' (Anchor contents config@AnchorConfig{..}) = do
    (e, a) <- reRestrict $ elWithAnim' "a" elConfig $ reRestrict contents
    return (e, AnchorResult (domEvent Click e) a)
      where
        elConfig = _config
          & elConfigAttributes %~ (\a -> (maybe id (M.insert "href") <$> _href) <*> a)

instance (t ~ t', m ~ m') => UI t' m' Inline (Anchor t m a) where
  ui' = unRestrict . ui'

data HeaderConfig t m b = HeaderConfig
  { _subHeader    :: Maybe (Restrict Inline m b)

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

instance Default (HeaderConfig t m b) where
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

headerConfigClasses :: Reflex t => HeaderConfig t m b -> Active t Classes
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

-- | Create a header.
--
-- https://semantic-ui.com/elements/header.html
data Header t m a b = Header
  { _size :: HeaderSize
  , _content :: Restrict Inline m a
  , _config :: HeaderConfig t m b
  }

data HeaderResult a b = HeaderResult
  { _content :: a
  , _subHeader :: Maybe b
  }

{-
instance ToItem (Header t m a b) where
  toItem (Header size content config)
    = Header size content config { _item = True, _component = False }

instance ToPart (Header t m a b) where
  toPart (Header size content config)
    = Header size content config { _component = False }
-}

instance (m ~ m', t ~ t') => UI t' m' None (Header t m a b) where
  type Return t' m' (Header t m a b) = HeaderResult a b
  ui' (Header size widget config@HeaderConfig {..}) = case _header of
    PageHeader -> reRestrict $ elWithAnim' (headerSizeEl size) attrs $ reRestrict content
    ContentHeader -> reRestrict $ elWithAnim' "div" attrs' $ reRestrict content
      where
        attrs' = attrs { _classes = addClass (headerSizeText size) <$> classes }
    where
      attrs = _config <> def
        { _classes = classes
        }
      classes = headerConfigClasses config
      content = do
        unRestrict $ runRenderWhen ui' _image
        unRestrict $ runRenderWhen ui' _icon
        divClass "content" `mapRestrict` do
          a <- widget
          mb <- traverse (divClass "sub header" `mapRestrict`) _subHeader
          return $ HeaderResult a mb
