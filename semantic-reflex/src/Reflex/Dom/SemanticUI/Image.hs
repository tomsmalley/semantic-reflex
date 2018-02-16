{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- | Semantic-UI Image elements
-- https://semantic-ui.com/elements/image.html
module Reflex.Dom.SemanticUI.Image where

import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
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
  { _imageInline :: Active t Bool
  -- ^ (default: 'False') Images can appear inline
  , _imageSize :: Active t (Maybe Size)
  -- ^ (default: 'Nothing') Images can have a different size
  , _imageShape :: Active t (Maybe ImageShape)
  -- ^ (default: 'Nothing') Images can have a different shape
  , _imageFloated :: Active t (Maybe Floated)
  -- ^ (default: 'Nothing') Images can be floated
  , _imageSpaced :: Active t (Maybe Spaced)
  -- ^ (default: 'Nothing') Images can have horizontal spacing
  , _imageElConfig :: ActiveElConfig t
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''ImageConfig

instance HasElConfig t (ImageConfig t) where
  elConfig = imageElConfig

instance Reflex t => Default (ImageConfig t) where
  def = ImageConfig
    { _imageInline = pure False
    , _imageSize = pure Nothing
    , _imageShape = pure Nothing
    , _imageFloated = pure Nothing
    , _imageSpaced = pure Nothing
    , _imageElConfig = def
    }

-- | Create the classes for 'image's
imageConfigClasses :: Reflex t => ImageConfig t -> Active t Classes
imageConfigClasses ImageConfig {..} = dynClasses
  [ pure $ Just $ "ui image"
  , fmap toClassText <$> _imageSize
  , fmap toClassText <$> _imageShape
  , fmap toClassText <$> _imageFloated
  , fmap toClassText <$> _imageSpaced
  , boolClass "inline" _imageInline
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
image' config@ImageConfig {..} eImg = fmap fst $ case eImg of
  Left _ -> uiElement' "img" elConf blank
  Right m -> uiElement' "div" elConf m
  where elConf = _imageElConfig <> def
          { _classes = imageConfigClasses config
          , _attrs = case eImg of
            Left (Img src conf) -> imgConfigAttrs src conf
            Right _ -> mempty
          }

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

-- | The 'img' function packaged into a type.
data Img t = Img
  { _imgSrc :: Active t Text
  , _imgConfig :: ImgConfig t
  }

-- | Optional configuration for 'img'
data ImgConfig t = ImgConfig
  { _imgTitle :: Active t (Maybe Text)
  , _imgAlt :: Active t (Maybe Text)
  }

instance Reflex t => Default (ImgConfig t) where
  def = ImgConfig
    { _imgTitle = pure Nothing
    , _imgAlt = pure Nothing
    }

-- | Create the @img@ attributes 'Map'.
imgConfigAttrs
  :: Reflex t
  => Active t Text  -- ^ @src@ attribute text
  -> ImgConfig t    -- ^ Optional config values
  -> Active t (Map Text Text)
imgConfigAttrs src ImgConfig {..} = mkAttrs <$> src <*> _imgTitle <*> _imgAlt
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
img' src config = fst <$> uiElement' "img" elConf blank
  where elConf = def { _attrs = imgConfigAttrs src config }

