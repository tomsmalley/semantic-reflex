{-# LANGUAGE RecursiveDo #-}

-- | Semantic UI messages. Pure reflex implementation is provided.
-- https://semantic-ui.com/collections/messages.html
module Reflex.Dom.SemanticUI.Message
  (

    message, message'
  , dismissableMessage, dismissableMessage'
  , MessageType (..)
  , MessageConfig (..)
  , messageConfig_floating
  , messageConfig_compact
  , messageConfig_attached
  , messageConfig_type
  , messageConfig_color
  , messageConfig_size
  , messageConfig_icon
  , messageConfig_elConfig

  ) where

#ifdef USE_TEMPLATE_HASKELL
import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
#else
import Control.Lens.Type
#endif

import Control.Lens ((%~), (?~))
import Data.Default
import Data.Semigroup ((<>))
import Reflex
import Reflex.Dom.Core

import Reflex.Active
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
  { _messageConfig_floating :: Active t Bool
  -- ^ Messages can be floating (note: not the same as float: left|right)
  , _messageConfig_compact :: Active t Bool
  -- ^ If the message should be compact

  , _messageConfig_attached :: Active t (Maybe VerticalAttached)
  -- ^ Messages can be attached vertically
  , _messageConfig_type :: Active t (Maybe MessageType)
  -- ^ Message type (essentially more color choices)
  , _messageConfig_color :: Active t (Maybe Color)
  -- ^ Message color
  , _messageConfig_size :: Active t (Maybe Size)
  -- ^ Message size

  , _messageConfig_icon :: Maybe (Icon t)
  -- ^ Messages have a main icon
  , _messageConfig_elConfig :: ActiveElConfig t
  -- ^ Config
  }
#ifdef USE_TEMPLATE_HASKELL
makeLensesWith (lensRules & simpleLenses .~ True) ''MessageConfig
#endif

instance HasElConfig t (MessageConfig t) where
  elConfig = messageConfig_elConfig

instance Reflex t => Default (MessageConfig t) where
  def = MessageConfig
    { _messageConfig_icon = Nothing

    , _messageConfig_floating = pure False
    , _messageConfig_attached = pure Nothing
    , _messageConfig_compact = pure False
    , _messageConfig_type = pure Nothing
    , _messageConfig_color = pure Nothing
    , _messageConfig_size = pure Nothing
    , _messageConfig_elConfig = def
    }

-- | Make the message div classes from the configuration
messageConfigClasses :: Reflex t => MessageConfig t -> Active t Classes
messageConfigClasses MessageConfig {..} = dynClasses
  [ pure $ Just "ui message"
  , pure $ "icon" <$ _messageConfig_icon
  , boolClass "floating" _messageConfig_floating
  , fmap toClassText <$> _messageConfig_attached
  , boolClass "compact" _messageConfig_compact
  , fmap toClassText <$> _messageConfig_type
  , fmap toClassText <$> _messageConfig_color
  , fmap toClassText <$> _messageConfig_size
  ]

message :: UI t m => MessageConfig t -> m a -> m a
message c = fmap snd . message' c

-- | Message UI Element. The minimum useful message only needs a label and a
-- default configuration.
message'
  :: UI t m => MessageConfig t -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
message' config@MessageConfig{..} content
  = ui' "div" elConf $ case _messageConfig_icon of
    Just (Icon i c) -> do
      icon i c
      divClass "content" content
    Nothing -> content
  where
    elConf = _messageConfig_elConfig <> def
      { _classes = messageConfigClasses config }

dismissableMessage
  :: UI t m => TransitionOrAnimation -> MessageConfig t -> m a -> m a
dismissableMessage t c = fmap snd . dismissableMessage' t c

dismissableMessage'
  :: UI t m => TransitionOrAnimation -> MessageConfig t -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
dismissableMessage' t config@MessageConfig{..} content = do
  rec
    let config' = config & action %~ (\old -> old <> (Just $ def
          & action_event ?~ closeEvent))

    (divEl, (result, closeEvent)) <- message' config' $ do
      e <- icon' "close" def
      ffor content $ \a -> (a, t <$ domEvent Click e)

  return (divEl, result)

#ifndef USE_TEMPLATE_HASKELL
#include "Message.th.hs"
#endif
