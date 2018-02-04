{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Semantic UI messages. Pure reflex implementation is provided.
-- https://semantic-ui.com/collections/messages.html
module Reflex.Dom.SemanticUI.Message
  (

    message, message'
  , dismissableMessage, dismissableMessage'
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

import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
import Control.Lens ((%~), (?~))
import Data.Default
import Data.Semigroup ((<>))
import Reflex
import Reflex.Dom.Core

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
  { _messageFloating :: Dynamic t Bool
  -- ^ Messages can be floating (note: not the same as float: left|right)
  , _messageCompact :: Dynamic t Bool
  -- ^ If the message should be compact

  , _messageAttached :: Dynamic t (Maybe VerticalAttached)
  -- ^ Messages can be attached vertically
  , _messageType :: Dynamic t (Maybe MessageType)
  -- ^ Message type (essentially more color choices)
  , _messageColor :: Dynamic t (Maybe Color)
  -- ^ Message color
  , _messageSize :: Dynamic t (Maybe Size)
  -- ^ Message size

  , _messageDismissable :: Maybe Transition
  -- ^ Messages can be dismissable using the given transition
  , _messageIcon :: Maybe (Icon t)
  -- ^ Messages have a main icon
  , _messageElConfig :: ActiveElConfig t
  -- ^ Config
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''MessageConfig

instance HasElConfig t (MessageConfig t) where
  elConfig = messageElConfig

instance Reflex t => Default (MessageConfig t) where
  def = MessageConfig
    { _messageDismissable = Nothing
    , _messageIcon = Nothing

    , _messageFloating = pure False
    , _messageAttached = pure Nothing
    , _messageCompact = pure False
    , _messageType = pure Nothing
    , _messageColor = pure Nothing
    , _messageSize = pure Nothing
    , _messageElConfig = def
    }

-- | Make the message div classes from the configuration
messageConfigClasses :: Reflex t => MessageConfig t -> Dynamic t Classes
messageConfigClasses MessageConfig {..} = dynClasses
  [ pure $ Just "ui message"
  , pure $ "icon" <$ _messageIcon
  , boolClass "floating" _messageFloating
  , fmap toClassText <$> _messageAttached
  , boolClass "compact" _messageCompact
  , fmap toClassText <$> _messageType
  , fmap toClassText <$> _messageColor
  , fmap toClassText <$> _messageSize
  ]

message :: MonadWidget t m => MessageConfig t -> m a -> m a
message c = fmap snd . message' c

-- | Message UI Element. The minimum useful message only needs a label and a
-- default configuration.
message' :: MonadWidget t m => MessageConfig t -> m a -> m (El t, a)
message' config@MessageConfig{..} content = do
  uiElement' "div" elConf $ case _messageIcon of
    Just (Icon i c) -> do
      icon i c
      divClass "content" content
    Nothing -> content
  where
    elConf = _messageElConfig <> def
      { _classes = messageConfigClasses config }

dismissableMessage
  :: MonadWidget t m => Transition -> MessageConfig t -> m a -> m a
dismissableMessage t c = fmap snd . dismissableMessage' t c

dismissableMessage'
  :: MonadWidget t m => Transition -> MessageConfig t -> m a -> m (El t, a)
dismissableMessage' t config@MessageConfig{..} content = do
  rec
    let config' = config & action %~ (\old -> old <> (Just $ def
          & actionEvent ?~ closeEvent))

    (divEl, (result, closeEvent)) <- message' config' $ do
      e <- icon' "close" def
      ffor content $ \a -> (a, t <$ domEvent Click e)

  return (divEl, result)

