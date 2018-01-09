{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Reflex.Dom.SemanticUI.Header
  (

  -- * Header
    header, header'
  , pageHeader, pageHeader'
  , subheader, subheader'
  , HeaderSize (..)
  , HeaderConfig (..)
  , headerLargeIcon
  , headerIcon
  , headerImage
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
  , headerComponent
  , headerItem
  , headerElConfig

  ) where

import Control.Lens (makeLenses)
import Data.Default
import Data.Semigroup ((<>))
import Data.Text (Text)
import Reflex
import Reflex.Dom.Core

import Reflex.Dom.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Icon (Icon(Icon), icon)
import Reflex.Dom.SemanticUI.Image (Image(Image), image)
import Reflex.Dom.SemanticUI.Transition

-- | Valid sizes of headers. We can't use 'Size' because the content header css
-- only implements 5 specific sizes.
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

data HeaderConfig t = HeaderConfig
  { _headerLargeIcon  :: Active t Bool
  , _headerDividing   :: Active t Bool
  , _headerSub        :: Active t Bool
  , _headerDisabled   :: Active t Bool
  , _headerBlock      :: Active t Bool
  , _headerInverted   :: Active t Bool

  , _headerSize       :: Active t (Maybe HeaderSize)
  , _headerFloated    :: Active t (Maybe Floated)
  , _headerAligned    :: Active t (Maybe Aligned)
  , _headerColor      :: Active t (Maybe Color)
  , _headerAttached   :: Active t (Maybe VerticalAttached)

  , _headerIcon       :: Maybe (Icon t)
  , _headerImage      :: Maybe (Image t)

  , _headerComponent  :: Bool -- This controls the "ui" class
  , _headerItem       :: Bool
  , _headerElConfig   :: ActiveElConfig t
  }
makeLenses ''HeaderConfig

instance HasElConfig t (HeaderConfig t) where
  elConfig = headerElConfig

instance Reflex t => Default (HeaderConfig t) where
  def = HeaderConfig
    { _headerLargeIcon = pure False
    , _headerIcon = Nothing
    , _headerImage = Nothing

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

    , _headerComponent = False
    , _headerItem = False
    , _headerElConfig = def
    }

headerConfigClasses :: Reflex t => HeaderConfig t -> Active t Classes
headerConfigClasses HeaderConfig {..} = activeClasses
  [ Static $ Just "header"
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

  , boolClass "ui" $ Static $ not _headerComponent
  , boolClass "item" $ Static _headerItem
  ]

-- | Create a top level header
-- https://semantic-ui.com/elements/header.html
pageHeader'
  :: MonadWidget t m => HeaderSize -> HeaderConfig t -> m a -> m (El t, a)
pageHeader' size config@HeaderConfig {..} widget
  = element' (headerSizeEl size) elConf $ case _headerImage of
    Nothing -> case _headerIcon of
      Nothing -> widget
      Just (Icon i c) -> do
        icon i c
        divClass "content" widget
    Just (Image i c) -> do
      image i c
      divClass "content" widget
  where
    elConf = _headerElConfig <> def { _classes = headerConfigClasses config }

pageHeader :: MonadWidget t m => HeaderSize -> HeaderConfig t -> m a -> m a
pageHeader s c = fmap snd . pageHeader' s c

-- | Create a content header
-- https://semantic-ui.com/elements/header.html
header' :: MonadWidget t m => HeaderConfig t -> m a -> m (El t, a)
header' config@HeaderConfig {..} widget
  = element' "div" elConf $ case _headerImage of
    Nothing -> case _headerIcon of
      Nothing -> widget
      Just (Icon i c) -> do
        icon i c
        divClass "content" widget
    Just (Image i c) -> do
      image i c
      divClass "content" widget
  where
    msize = (fmap . fmap) headerSizeText _headerSize
    addSize = maybe id addClass <$> msize
    elConf = _headerElConfig <> def
      { _classes = addSize <*> headerConfigClasses config
      }

header :: MonadWidget t m => HeaderConfig t -> m a -> m a
header c = fmap snd . header' c

subheader' :: MonadWidget t m => m a -> m (El t, a)
subheader' = divClass' "sub header"

subheader :: MonadWidget t m => m a -> m a
subheader = fmap snd . subheader'

