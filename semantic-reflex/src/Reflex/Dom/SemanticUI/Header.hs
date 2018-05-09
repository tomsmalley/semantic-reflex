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
  , headerConfig_largeIcon
  , headerConfig_dividing
  , headerConfig_sub
  , headerConfig_disabled
  , headerConfig_block
  , headerConfig_inverted
  , headerConfig_size
  , headerConfig_floated
  , headerConfig_aligned
  , headerConfig_color
  , headerConfig_attached
  , headerConfig_elConfig
  , headerConfig_preContent

  ) where

#ifdef USE_TEMPLATE_HASKELL
import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
#else
import Control.Lens.Type
#endif

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
  { _headerConfig_largeIcon  :: Active t Bool
  -- ^ (default: 'False') Headers can emphasise an icon
  , _headerConfig_dividing   :: Active t Bool
  -- ^ (default: 'False') Headers can be dividing
  , _headerConfig_sub        :: Active t Bool
  -- ^ (default: 'False') Headers can label de-emphasised content
  , _headerConfig_disabled   :: Active t Bool
  -- ^ (default: 'False') Headers can be "disabled" (less emphasis)
  , _headerConfig_block      :: Active t Bool
  -- ^ (default: 'False') Headers can have a border around them
  , _headerConfig_inverted   :: Active t Bool
  -- ^ (default: 'False') Headers can have inverted colors

  , _headerConfig_size       :: Active t (Maybe HeaderSize)
  -- ^ (default: 'Nothing') Headers can have a different size. 'H3' equates to
  -- 'Medium'.
  , _headerConfig_floated    :: Active t (Maybe Floated)
  -- ^ (default: 'Nothing') Headers can be floated
  , _headerConfig_aligned    :: Active t (Maybe Aligned)
  -- ^ (default: 'Nothing') Headers can have different text alignment
  , _headerConfig_color      :: Active t (Maybe Color)
  -- ^ (default: 'Nothing') Headers can have a color
  , _headerConfig_attached   :: Active t (Maybe VerticalAttached)
  -- ^ (default: 'Nothing') Headers can be vertically attached to other content

  , _headerConfig_preContent :: Maybe (m ())
  -- ^ (default: 'Nothing') Widget placed before the main content, causes the
  -- main content to be placed into a div with class "content".
  , _headerConfig_elConfig   :: ActiveElConfig t
  }
#ifdef USE_TEMPLATE_HASKELL
makeLensesWith (lensRules & simpleLenses .~ True) ''HeaderConfig
#endif

instance HasElConfig t (HeaderConfig t m) where
  elConfig = headerConfig_elConfig

instance Reflex t => Default (HeaderConfig t m) where
  def = HeaderConfig
    { _headerConfig_largeIcon = pure False
    , _headerConfig_dividing = pure False
    , _headerConfig_sub = pure False
    , _headerConfig_disabled = pure False
    , _headerConfig_block = pure False
    , _headerConfig_inverted = pure False

    , _headerConfig_size = pure Nothing
    , _headerConfig_floated = pure Nothing
    , _headerConfig_aligned = pure Nothing
    , _headerConfig_color = pure Nothing
    , _headerConfig_attached = pure Nothing

    , _headerConfig_preContent = Nothing
    , _headerConfig_elConfig = def
    }

-- | Make the header classes
headerConfigClasses :: Reflex t => HeaderConfig t m -> Active t Classes
headerConfigClasses HeaderConfig {..} = dynClasses
  [ pure $ Just "ui header"

  , boolClass "icon" _headerConfig_largeIcon
  , boolClass "dividing" _headerConfig_dividing
  , boolClass "sub" _headerConfig_sub
  , boolClass "disabled" _headerConfig_disabled
  , boolClass "block" _headerConfig_block
  , boolClass "inverted" _headerConfig_inverted

  , fmap toClassText <$> _headerConfig_floated
  , fmap toClassText <$> _headerConfig_aligned
  , fmap toClassText <$> _headerConfig_color
  , fmap toClassText <$> _headerConfig_attached
  ]

-- | Header common implementation.
headerInternal
  :: UI t m => Maybe HeaderSize -> HeaderConfig t m -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
headerInternal mSize config@HeaderConfig {..} content
  = ui' elType elConf $ case _headerConfig_preContent of
    Nothing -> content
    Just m -> m >> divClass "content" content
  where
    elConf = _headerConfig_elConfig <> def
      { _classes = addSize <*> headerConfigClasses config }
    (elType, addSize) = case mSize of
      Just size -> (headerSizeEl size, pure id)
      Nothing -> (,) "div" $ maybe id addClass <$>
        (fmap . fmap) headerSizeText _headerConfig_size

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

#ifndef USE_TEMPLATE_HASKELL
#include "Header.th.hs"
#endif
