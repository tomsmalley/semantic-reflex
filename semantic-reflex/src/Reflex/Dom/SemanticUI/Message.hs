{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE RecursiveDo            #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}

-- | Semantic UI messages. Pure reflex implementation is provided.
-- https://semantic-ui.com/collections/messages.html
module Reflex.Dom.SemanticUI.Message where

import Data.Default
import Reflex

import Reflex.Dom.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Icon (Icon)
import Reflex.Dom.SemanticUI.Transition (Transition, ActiveElConfig)

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
  { _dismissable :: Maybe Transition
  -- ^ Messages can be dismissable using the given transition
  , _icon :: Maybe (Icon t)
  -- ^ Messages have a main icon

  , _floating :: Active t Bool
  -- ^ Messages can be floating (note: not the same as float: left|right)
  , _attached :: Active t (Maybe VerticalAttached)
  -- ^ Messages can be attached vertically
  , _compact :: Active t Bool
  -- ^ If the message should be compact
  , _messageType :: Active t (Maybe MessageType)
  -- ^ Message type (essentially more color choices)
  , _positive :: Active t (Maybe Positive)
  -- ^ Message success / error
  , _color :: Active t (Maybe Color)
  -- ^ Message color
  , _size :: Active t (Maybe Size)
  -- ^ Message size
  , _config :: ActiveElConfig t
  -- ^ Config
  }

instance Reflex t => Default (MessageConfig t) where
  def = MessageConfig
    { _dismissable = Nothing
    , _icon = Nothing

    , _floating = Static False
    , _attached = Static Nothing
    , _compact = Static False
    , _messageType = Static Nothing
    , _positive = Static Nothing
    , _color = Static Nothing
    , _size = Static Nothing
    , _config = def
    }

-- | Make the message div classes from the configuration
messageConfigClasses :: Reflex t => MessageConfig t -> Active t Classes
messageConfigClasses MessageConfig {..} = activeClasses
  [ Static $ Just "ui message"
  , Static $ "icon" <$ _icon
  , boolClass "floating" _floating
  , fmap toClassText <$> _attached
  , boolClass "compact" _compact
  , fmap toClassText <$> _messageType
  , fmap toClassText <$> _positive
  , fmap toClassText <$> _color
  , fmap toClassText <$> _size
  ]

-- | Message UI Element. The minimum useful message only needs a label and a
-- default configuration.
data Message t m a = Message
  { _config :: MessageConfig t
  , _content :: Component Message m a
  }
