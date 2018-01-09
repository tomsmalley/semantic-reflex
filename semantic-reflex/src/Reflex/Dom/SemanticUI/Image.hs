{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Reflex.Dom.SemanticUI.Image
  (

    image, image'
  , Image (..)
  , ImageConfig (..)
  , imageInline
  , imageSize
  , imageShape
  , imageFloated
  , imageTitle
  , imageSpaced
  , imageComponent
  , imageElConfig

  ) where

import Control.Lens (makeLenses)
import Control.Monad (void)
import Data.Default
import Data.Semigroup ((<>))
import Data.Text (Text)
import Reflex
import Reflex.Dom.Core

import Reflex.Dom.Active
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
  { _imageInline :: Active t Bool

  , _imageSize :: Active t (Maybe Size)
  , _imageShape :: Active t (Maybe ImageShape)
  , _imageFloated :: Active t (Maybe Floated)
  , _imageTitle :: Active t (Maybe Text)
  , _imageSpaced :: Active t (Maybe Spaced)

  , _imageComponent :: Bool
  , _imageElConfig :: ActiveElConfig t
  }
makeLenses ''ImageConfig

instance Reflex t => Default (ImageConfig t) where
  def = ImageConfig
    { _imageSize = Static Nothing
    , _imageShape = Static Nothing
    , _imageFloated = Static Nothing
    , _imageComponent = False
    , _imageTitle = Static Nothing
    , _imageSpaced = Static Nothing
    , _imageInline = Static False
    , _imageElConfig = def
    }

imageConfigClasses :: Reflex t => ImageConfig t -> Active t Classes
imageConfigClasses ImageConfig {..} = activeClasses
  [ Static $ justWhen (not _imageComponent) "ui image"
  , fmap toClassText <$> _imageSize
  , fmap toClassText <$> _imageShape
  , fmap toClassText <$> _imageFloated
  , fmap toClassText <$> _imageSpaced
  , boolClass "inline" _imageInline
  ]

data Image t = Image
  { _imageSrc :: Active t Text
  , _imageConfig :: ImageConfig t
  }

data ContentImage t m a = ContentImage
  { _contentImageSrc :: Active t Text
  , _contentImageConfig :: ImageConfig t
  , _contentImageContent :: m a
  }

image' :: MonadWidget t m => Active t Text -> ImageConfig t -> m (El t)
image' src config@ImageConfig {..} = fst <$> element' "img" elConf blank
  where
    elConf = _imageElConfig <> def
      { _classes = imageConfigClasses config
      , _attrs = mkAttrs <$> src <*> _imageTitle
      }
    mkAttrs s t = "src" =: s <> maybe mempty ("title" =:) t

image :: MonadWidget t m => Active t Text -> ImageConfig t -> m ()
image i = void . image' i

contentImage'
  :: MonadWidget t m => Active t Text -> ImageConfig t -> m a -> m (El t, a)
contentImage' src config@ImageConfig {..} content
  = element' "div" elConf $ do
    a <- content
    void $ element' "img" imgConfig blank
    return a
  where
    elConf = _imageElConfig <> def
      { _classes = imageConfigClasses config }
    imgConfig = def { _attrs = mkAttrs <$> src <*> _imageTitle
                    , _classes = imageConfigClasses config }
    mkAttrs s t = "src" =: s <> maybe mempty ("title" =:) t

contentImage :: MonadWidget t m => Active t Text -> ImageConfig t -> m a -> m a
contentImage i config = fmap snd . contentImage' i config

{-
-- | Images in labels *must* be some form of spaced if they are not an 'Avatar',
-- or they will cause a line break. Default to spacing both sides allowing user
-- override to 'LeftSpaced' or 'RightSpaced'.
instance t ~ t' => Render t' m (Image t) where
  type Return t' m (Image t) = ()
  ui' (Image url conf@ImageConfig{..}) = ui' $ Image url conf'
    where conf' = conf { _spaced = mkSpaced <$> _shape <*> _spaced }
          mkSpaced mShape mSpaced = if mShape == Just Avatar then mSpaced
                                    else Just $ fromMaybe Spaced mSpaced
-}

