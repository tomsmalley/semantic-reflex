{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE RecursiveDo            #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}

-- | Semantic UI messages. Pure reflex implementation is provided.
-- https://semantic-ui.com/collections/messages.html
module Reflex.Dom.SemanticUI.Message
  (

    message, message'
  , MessageType (..)
  , MessageConfig (..)
  , messageFloating
  , messageCompact
  , messageAttached
  , messageType
  , messageColor
  , messageSize
  , messageDismissable
  , messageIcon
  , messageElConfig

  ) where

import Control.Lens
import Data.Default
import Data.Semigroup ((<>))
import Reflex
import Reflex.Dom.Core

import Reflex.Dom.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Icon (Icon(Icon), icon, icon')
import Reflex.Dom.SemanticUI.Transition

data MessageType
  = WarningMessage
  | InfoMessage
  | MessageType Positive
  deriving (Eq, Show)

instance ToClassText MessageType where
  toClassText WarningMessage = "warning"
  toClassText InfoMessage = "info"
  toClassText (MessageType p) = toClassText p

-- | Configuration of a message.
data MessageConfig t = MessageConfig
  { _messageFloating :: Active t Bool
  -- ^ Messages can be floating (note: not the same as float: left|right)
  , _messageCompact :: Active t Bool
  -- ^ If the message should be compact

  , _messageAttached :: Active t (Maybe VerticalAttached)
  -- ^ Messages can be attached vertically
  , _messageType :: Active t (Maybe MessageType)
  -- ^ Message type (essentially more color choices)
  , _messageColor :: Active t (Maybe Color)
  -- ^ Message color
  , _messageSize :: Active t (Maybe Size)
  -- ^ Message size

  , _messageDismissable :: Maybe Transition
  -- ^ Messages can be dismissable using the given transition
  , _messageIcon :: Maybe (Icon t)
  -- ^ Messages have a main icon
  , _messageElConfig :: ActiveElConfig t
  -- ^ Config
  }
makeLenses ''MessageConfig

instance HasElConfig t (MessageConfig t) where
  elConfig = messageElConfig

instance Reflex t => Default (MessageConfig t) where
  def = MessageConfig
    { _messageDismissable = Nothing
    , _messageIcon = Nothing

    , _messageFloating = Static False
    , _messageAttached = Static Nothing
    , _messageCompact = Static False
    , _messageType = Static Nothing
    , _messageColor = Static Nothing
    , _messageSize = Static Nothing
    , _messageElConfig = def
    }

-- | Make the message div classes from the configuration
messageConfigClasses :: Reflex t => MessageConfig t -> Active t Classes
messageConfigClasses MessageConfig {..} = activeClasses
  [ Static $ Just "ui message"
  , Static $ "icon" <$ _messageIcon
  , boolClass "floating" _messageFloating
  , fmap toClassText <$> _messageAttached
  , boolClass "compact" _messageCompact
  , fmap toClassText <$> _messageType
  , fmap toClassText <$> _messageColor
  , fmap toClassText <$> _messageSize
  ]

-- | Message UI Element. The minimum useful message only needs a label and a
-- default configuration.
message'
  :: forall t m a. MonadWidget t m => MessageConfig t -> m a -> m (El t, a)
message' config@MessageConfig{..} content = do

  case _messageDismissable of
    Nothing -> uiElement' "div" (elConf id) $ case _messageIcon of
      Just (Icon i c) -> do
        icon i c
        divClass "content" content
      Nothing -> content

    Just t -> do

      let dismissContent = do
            e <- icon' "close" def
            result <- content
            return (result, t <$ domEvent Click e)

      rec

        let elConfig' = elConf $ set transition $ Just $ def
              & transConfigEvent .~ closeEvent

        (divEl, (result, closeEvent)) <- uiElement' "div" elConfig' $
          case _messageIcon of
            Just (Icon i c) -> do
              icon i c
              divClass "content" dismissContent
            Nothing -> dismissContent

      return (divEl, result)

  where
    elConf f = _messageElConfig <> (f def)
      { _classes = messageConfigClasses config }

message :: MonadWidget t m => MessageConfig t -> m a -> m a
message c = fmap snd . message' c

