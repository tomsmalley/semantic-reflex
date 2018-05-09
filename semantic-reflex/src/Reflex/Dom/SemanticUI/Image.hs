{-# LANGUAGE TupleSections #-}

-- | Semantic-UI Image elements
-- https://semantic-ui.com/elements/image.html
module Reflex.Dom.SemanticUI.Image where

#ifdef USE_TEMPLATE_HASKELL
import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
#else
import Control.Lens.Type
#endif

import Control.Monad (void)
import Data.Default
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Reflex
import Reflex.Dom.Core

import Reflex.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

import qualified Data.Map as M

-- | Images can have a different shape
data ImageShape = Rounded | Circular | Avatar deriving (Eq, Show)

instance ToClassText ImageShape where
  toClassText Rounded = "rounded"
  toClassText Circular = "circular"
  toClassText Avatar = "avatar"

-- | Images can have a particular spacing to the sides
data Spaced = LeftSpaced | Spaced | RightSpaced deriving (Eq, Show)

instance ToClassText Spaced where
  toClassText LeftSpaced = "left spaced"
  toClassText Spaced = "spaced"
  toClassText RightSpaced = "right spaced"

-- | Config for 'image's
data ImageConfig t = ImageConfig
  { _imageConfig_inline :: Active t Bool
  -- ^ (default: 'False') Images can appear inline
  , _imageConfig_size :: Active t (Maybe Size)
  -- ^ (default: 'Nothing') Images can have a different size
  , _imageConfig_shape :: Active t (Maybe ImageShape)
  -- ^ (default: 'Nothing') Images can have a different shape
  , _imageConfig_floated :: Active t (Maybe Floated)
  -- ^ (default: 'Nothing') Images can be floated
  , _imageConfig_spaced :: Active t (Maybe Spaced)
  -- ^ (default: 'Nothing') Images can have horizontal spacing
  , _imageConfig_elConfig :: ActiveElConfig t
  }
#ifdef USE_TEMPLATE_HASKELL
makeLensesWith (lensRules & simpleLenses .~ True) ''ImageConfig
#endif

instance HasElConfig t (ImageConfig t) where
  elConfig = imageConfig_elConfig

instance Reflex t => Default (ImageConfig t) where
  def = ImageConfig
    { _imageConfig_inline = pure False
    , _imageConfig_size = pure Nothing
    , _imageConfig_shape = pure Nothing
    , _imageConfig_floated = pure Nothing
    , _imageConfig_spaced = pure Nothing
    , _imageConfig_elConfig = def
    }

-- | Create the classes for 'image's
imageConfigClasses :: Reflex t => ImageConfig t -> Active t Classes
imageConfigClasses ImageConfig {..} = dynClasses
  [ pure $ Just $ "ui image"
  , fmap toClassText <$> _imageConfig_size
  , fmap toClassText <$> _imageConfig_shape
  , fmap toClassText <$> _imageConfig_floated
  , fmap toClassText <$> _imageConfig_spaced
  , boolClass "inline" _imageConfig_inline
  ]

-- | Create a Semantic-UI image, returning the 'Element'.
image'
  :: UI t m
  => ImageConfig t
  -- ^ Image config
  -> Either (Img t) (m ())
  -- ^ 'Left' creates an @img@ element similar to 'img' but with the classes of
  -- a Semantic-UI image element. 'Right' creates a @div@ element with the
  -- classes of a Semantic-UI image element which wraps the given content. The
  -- content should include an 'img' somewhere.
  -> m (Element EventResult (DomBuilderSpace m) t)
image' config@ImageConfig {..} = \case
  Left (Img src imgConf) -> img' src $ imgConf
    { _imgConfig_elConfig = _imgConfig_elConfig imgConf <> elConf}
  Right m -> fmap fst $ ui' "div" elConf m
  where elConf = _imageConfig_elConfig <> def { _classes = imageConfigClasses config }

-- | Create a Semantic-UI image.
image
  :: UI t m
  => ImageConfig t
  -- ^ Image config
  -> Either (Img t) (m ())
  -- ^ 'Left' creates an @img@ element similar to 'img' but with the classes of
  -- a Semantic-UI image element. 'Right' creates a @div@ element with the
  -- classes of a Semantic-UI image element which wraps the given content. The
  -- content should include an 'img' somewhere.
  -> m ()
image i = void . image' i

-- | Create the @img@ attributes 'Map'.
imgConfigAttrs
  :: Reflex t
  => Active t Text  -- ^ @src@ attribute text
  -> ImgConfig t    -- ^ Optional config values
  -> Active t (Map Text Text)
imgConfigAttrs src ImgConfig {..} = mkAttrs <$> src <*> _imgConfig_title <*> _imgConfig_alt
  where
    mkAttrs s t a = M.fromList $ catMaybes
      [ Just ("src", s)
      , ("title",) <$> t
      , ("alt",) <$> a
      ]

-- | Create an @img@ element with the given @src@. This is just a plain @img@
-- element with no Semantic-UI classes.
img
  :: UI t m
  => Active t Text  -- ^ @src@ attribute text
  -> ImgConfig t    -- ^ Optional config
  -> m ()
img src = void . img' src

-- | Create an @img@ element with the given @src@, returning the 'Element'. This
-- is just a plain @img@ element with no Semantic-UI classes.
img'
  :: UI t m
  => Active t Text  -- ^ @src@ attribute text
  -> ImgConfig t    -- ^ Optional config
  -> m (Element EventResult (DomBuilderSpace m) t)
img' src config@ImgConfig {..} = fst <$> ui' "img" elConf blank
  where elConf = _imgConfig_elConfig <> def { _attrs = imgConfigAttrs src config }

-- | The 'img' function packaged into a type.
data Img t = Img
  { _img_src :: Active t Text
  , _img_config :: ImgConfig t
  }

-- | Optional configuration for 'img'
data ImgConfig t = ImgConfig
  { _imgConfig_title :: Active t (Maybe Text)
  , _imgConfig_alt :: Active t (Maybe Text)
  , _imgConfig_elConfig :: ActiveElConfig t
  }
#ifdef USE_TEMPLATE_HASKELL
makeLensesWith (lensRules & simpleLenses .~ True) ''ImgConfig
#endif

instance HasElConfig t (ImgConfig t) where
  elConfig = imgConfig_elConfig

instance Reflex t => Default (ImgConfig t) where
  def = ImgConfig
    { _imgConfig_title = pure Nothing
    , _imgConfig_alt = pure Nothing
    , _imgConfig_elConfig = def
    }

#ifndef USE_TEMPLATE_HASKELL
#include "Image.th.hs"
#endif
