{-# LANGUAGE TemplateHaskell #-}

module Reflex.Dom.SemanticUI.Image
  (

    image, image'
  , contentImage, contentImage'
  , Image (..)
  , ContentImage (..)
  , ImageConfig (..)
  , ImageShape (..)
  , Spaced (..)
  , imageInline
  , imageSize
  , imageShape
  , imageFloated
  , imageTitle
  , imageSpaced
  , imageComponent
  , imageElConfig

  ) where

import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
import Control.Monad (void, guard)
import Data.Default
import Data.Semigroup ((<>))
import Data.Text (Text)
import Reflex
import Reflex.Dom.Core

import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data ImageShape = Rounded | Circular | Avatar deriving (Eq, Show)

instance ToClassText ImageShape where
  toClassText Rounded = "rounded"
  toClassText Circular = "circular"
  toClassText Avatar = "avatar"

data Spaced = LeftSpaced | Spaced | RightSpaced deriving (Eq, Show)

instance ToClassText Spaced where
  toClassText LeftSpaced = "left spaced"
  toClassText Spaced = "spaced"
  toClassText RightSpaced = "right spaced"

data ImageConfig t = ImageConfig
  { _imageInline :: Dynamic t Bool

  , _imageSize :: Dynamic t (Maybe Size)
  , _imageShape :: Dynamic t (Maybe ImageShape)
  , _imageFloated :: Dynamic t (Maybe Floated)
  , _imageTitle :: Dynamic t (Maybe Text)
  , _imageSpaced :: Dynamic t (Maybe Spaced)

  , _imageComponent :: Bool
  , _imageElConfig :: ActiveElConfig t
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''ImageConfig

instance HasElConfig t (ImageConfig t) where
  elConfig = imageElConfig

instance Reflex t => Default (ImageConfig t) where
  def = ImageConfig
    { _imageSize = pure Nothing
    , _imageShape = pure Nothing
    , _imageFloated = pure Nothing
    , _imageComponent = False
    , _imageTitle = pure Nothing
    , _imageSpaced = pure Nothing
    , _imageInline = pure False
    , _imageElConfig = def
    }

imageConfigClasses :: Reflex t => ImageConfig t -> Dynamic t Classes
imageConfigClasses ImageConfig {..} = dynClasses
  [ pure $ "ui image" <$ guard (not _imageComponent)
  , fmap toClassText <$> _imageSize
  , fmap toClassText <$> _imageShape
  , fmap toClassText <$> _imageFloated
  , fmap toClassText <$> _imageSpaced
  , boolClass "inline" _imageInline
  ]

data Image t = Image
  { _imageSrc :: Dynamic t Text
  , _imageConfig :: ImageConfig t
  }

data ContentImage t m a = ContentImage
  { _contentImageSrc :: Dynamic t Text
  , _contentImageConfig :: ImageConfig t
  , _contentImageContent :: m a
  }

image' :: MonadWidget t m => Dynamic t Text -> ImageConfig t -> m (El t)
image' src config@ImageConfig {..} = fst <$> uiElement' "img" elConf blank
  where
    elConf = _imageElConfig <> def
      { _classes = imageConfigClasses config
      , _attrs = mkAttrs <$> src <*> _imageTitle
      }
    mkAttrs s t = "src" =: s <> maybe mempty ("title" =:) t

image :: MonadWidget t m => Dynamic t Text -> ImageConfig t -> m ()
image i = void . image' i

contentImage'
  :: MonadWidget t m => Dynamic t Text -> ImageConfig t -> m a -> m (El t, a)
contentImage' src config@ImageConfig {..} content
  = uiElement' "div" elConf $ do
    a <- content
    void $ uiElement' "img" imgConfig blank
    return a
  where
    elConf = _imageElConfig <> def
      { _classes = imageConfigClasses config }
    imgConfig = def { _attrs = mkAttrs <$> src <*> _imageTitle
                    , _classes = imageConfigClasses config }
    mkAttrs s t = "src" =: s <> maybe mempty ("title" =:) t

contentImage :: MonadWidget t m => Dynamic t Text -> ImageConfig t -> m a -> m a
contentImage i config = fmap snd . contentImage' i config

