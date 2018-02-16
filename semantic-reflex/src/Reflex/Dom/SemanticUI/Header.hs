{-# LANGUAGE TemplateHaskell #-}

-- | Semantic-UI Header elements
--
-- https://semantic-ui.com/elements/header.html
module Reflex.Dom.SemanticUI.Header
  (

  -- * Header config
    HeaderSize (..)
  , HeaderConfig (..)

  -- * Content headers
  , header, header'
  -- * Page headers
  , pageHeader, pageHeader'
  -- * Sub headers
  , subHeader, subHeader'

  -- * Lenses
  , headerLargeIcon
  , headerDividing
  , headerSub
  , headerDisabled
  , headerBlock
  , headerInverted
  , headerSize
  , headerFloated
  , headerAligned
  , headerColor
  , headerAttached
  , headerElConfig
  , headerPreContent

  ) where

import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
import Data.Default
import Data.Semigroup ((<>))
import Data.Text (Text)
import Reflex
import Reflex.Dom.Core

import Reflex.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

-- | Valid sizes of headers. We can't use 'Size' because the content header css
-- only implements 5 specific sizes.
data HeaderSize = H1 | H2 | H3 | H4 | H5 deriving (Eq, Show)

-- | 'HeaderSize' HTML element tag text
headerSizeEl :: HeaderSize -> Text
headerSizeEl H1 = "h1"
headerSizeEl H2 = "h2"
headerSizeEl H3 = "h3"
headerSizeEl H4 = "h4"
headerSizeEl H5 = "h5"

-- | Convert 'HeaderSize' to corresponding size
headerSizeText :: HeaderSize -> Text
headerSizeText H1 = "huge"
headerSizeText H2 = "large"
headerSizeText H3 = "medium"
headerSizeText H4 = "small"
headerSizeText H5 = "tiny"

-- | Optional configuration settings for 'header's
data HeaderConfig t m = HeaderConfig
  { _headerLargeIcon  :: Active t Bool
  -- ^ (default: 'False') Headers can emphasise an icon
  , _headerDividing   :: Active t Bool
  -- ^ (default: 'False') Headers can be dividing
  , _headerSub        :: Active t Bool
  -- ^ (default: 'False') Headers can label de-emphasised content
  , _headerDisabled   :: Active t Bool
  -- ^ (default: 'False') Headers can be "disabled" (less emphasis)
  , _headerBlock      :: Active t Bool
  -- ^ (default: 'False') Headers can have a border around them
  , _headerInverted   :: Active t Bool
  -- ^ (default: 'False') Headers can have inverted colors

  , _headerSize       :: Active t (Maybe HeaderSize)
  -- ^ (default: 'Nothing') Headers can have a different size. 'H3' equates to
  -- 'Medium'.
  , _headerFloated    :: Active t (Maybe Floated)
  -- ^ (default: 'Nothing') Headers can be floated
  , _headerAligned    :: Active t (Maybe Aligned)
  -- ^ (default: 'Nothing') Headers can have different text alignment
  , _headerColor      :: Active t (Maybe Color)
  -- ^ (default: 'Nothing') Headers can have a color
  , _headerAttached   :: Active t (Maybe VerticalAttached)
  -- ^ (default: 'Nothing') Headers can be vertically attached to other content

  , _headerPreContent :: Maybe (m ())
  -- ^ (default: 'Nothing') Widget placed before the main content, causes the
  -- main content to be placed into a div with class "content".
  , _headerElConfig   :: ActiveElConfig t
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''HeaderConfig

instance HasElConfig t (HeaderConfig t m) where
  elConfig = headerElConfig

instance Reflex t => Default (HeaderConfig t m) where
  def = HeaderConfig
    { _headerLargeIcon = pure False
    , _headerDividing = pure False
    , _headerSub = pure False
    , _headerDisabled = pure False
    , _headerBlock = pure False
    , _headerInverted = pure False

    , _headerSize = pure Nothing
    , _headerFloated = pure Nothing
    , _headerAligned = pure Nothing
    , _headerColor = pure Nothing
    , _headerAttached = pure Nothing

    , _headerPreContent = Nothing
    , _headerElConfig = def
    }

-- | Make the header classes
headerConfigClasses :: Reflex t => HeaderConfig t m -> Active t Classes
headerConfigClasses HeaderConfig {..} = dynClasses
  [ pure $ Just "ui header"

  , boolClass "icon" _headerLargeIcon
  , boolClass "dividing" _headerDividing
  , boolClass "sub" _headerSub
  , boolClass "disabled" _headerDisabled
  , boolClass "block" _headerBlock
  , boolClass "inverted" _headerInverted

  , fmap toClassText <$> _headerFloated
  , fmap toClassText <$> _headerAligned
  , fmap toClassText <$> _headerColor
  , fmap toClassText <$> _headerAttached
  ]

-- | Header common implementation.
headerInternal
  :: UI t m => Maybe HeaderSize -> HeaderConfig t m -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
headerInternal mSize config@HeaderConfig {..} content
  = uiElement' elType elConf $ case _headerPreContent of
    Nothing -> content
    Just m -> m >> divClass "content" content
  where
    elConf = _headerElConfig <> def
      { _classes = addSize <*> headerConfigClasses config }
    (elType, addSize) = case mSize of
      Just size -> (headerSizeEl size, pure id)
      Nothing -> (,) "div" $ maybe id addClass <$>
        (fmap . fmap) headerSizeText _headerSize

-- | Create a top level header (uses HTML @header@ elements), returning the
-- 'Element'.
pageHeader'
  :: UI t m => HeaderSize -> HeaderConfig t m -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
pageHeader' size config = headerInternal (Just size) config

-- | Create a top level header (uses HTML @header@ elements).
pageHeader :: UI t m => HeaderSize -> HeaderConfig t m -> m a -> m a
pageHeader s c = fmap snd . pageHeader' s c

-- | Create a content header (uses HTML @div@ element), returning the 'Element'.
header'
  :: UI t m => HeaderConfig t m -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
header' config = headerInternal Nothing config

-- | Create a content header (uses HTML @div@ element).
header :: UI t m => HeaderConfig t m -> m a -> m a
header c = fmap snd . header' c

-- | Create a subheader, returning the 'Element'. A subheader can be placed
-- into a 'header's content.
subHeader' :: UI t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
subHeader' = divClass' "sub header"

-- | Create a subheader. A subheader can be placed into a 'header's content.
subHeader :: UI t m => m a -> m a
subHeader = fmap snd . subHeader'

