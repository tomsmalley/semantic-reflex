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
module Reflex.Dom.SemanticUI.Message
  (
  -- * Message
    Message (..)
  -- * Message type
  , MessageType (..)
  -- * Message result
  , MessageResult (..)
  -- * Message config
  , MessageConfig (..)
  ) where

import Control.Lens ((%~))
import Control.Monad ((<=<))
import Data.Default (Default(..))
import Data.Functor.Misc (WrapArg(..))
import Data.Maybe (isJust)
import Data.Proxy (Proxy(..))
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.HTMLInputElement as Input
import Language.Javascript.JSaddle (liftJSM)
import Reflex
import Reflex.Dom.Core hiding (message, Message, MessageConfig)

import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Icon

data MessageType
  = ErrorMessage
  | NegativeMessage
  | PositiveMessage
  | SuccessMessage
  | WarningMessage
  | InfoMessage
  deriving (Eq, Show)

instance ToClassText MessageType where
  toClassText ErrorMessage = "error"
  toClassText NegativeMessage = "negative"
  toClassText PositiveMessage = "positive"
  toClassText SuccessMessage = "success"
  toClassText WarningMessage = "warning"
  toClassText InfoMessage = "info"

-- | Configuration of a message. Value and indeterminate are split into initial
-- and set events in order to logically disconnect them from their dynamic
-- return values in MessageResult.
data MessageConfig t m a b = MessageConfig
  { _header :: Maybe (m a)
  -- ^ Message header content
  , _message :: Maybe (m b)
  -- ^ Message content
  , _icon :: Maybe (Icon t)
  -- ^ Message icon
  , _dismissable :: Bool
  -- ^ Messages can be dismissable
  , _setHidden :: Event t Bool
  -- ^ Messages can be hidden

  , _floating :: Active t Bool
  -- ^ Messages can be floating (note: not the same as float: left|right)
  , _attached :: Active t (Maybe VerticalAttached)
  -- ^ Messages can be attached vertically
  , _compact :: Active t Bool
  -- ^ If the message should be compact
  , _messageType :: Active t (Maybe MessageType)
  -- ^ Message type (essentially more color choices)
  , _color :: Active t (Maybe Color)
  -- ^ Message color
  , _size :: Active t (Maybe Size)
  -- ^ Message size
  }

instance Reflex t => Default (MessageConfig t m a b) where
  def = MessageConfig
    { _header = Nothing
    , _message = Nothing
    , _icon = Nothing
    , _dismissable = False
    , _setHidden = never

    , _floating = Static False
    , _attached = Static Nothing
    , _compact = Static False
    , _messageType = Static Nothing
    , _color = Static Nothing
    , _size = Static Nothing
    }

-- | Make the message div classes from the configuration
messageConfigClasses :: Reflex t => MessageConfig t m a b -> Active t ClassText
messageConfigClasses MessageConfig {..} = mconcat
  [ Static $ memptyUnless "icon" $ isJust _icon
  , memptyUnless "floating" <$> _floating
  , toClassText <$> _attached
  , memptyUnless "compact" <$> _compact
  , toClassText <$> _messageType
  , toClassText <$> _color
  , toClassText <$> _size
  ]

-- | Result of running a message
data MessageResult t m a b = MessageResult
  { _header :: Maybe a
  -- ^ The header return value
  , _message :: Maybe b
  -- ^ The message return value
  , _icon :: Maybe (El t, Return t m (Icon t))
  -- ^ Icon result
  }

-- | Message UI Element. The minimum useful message only needs a label and a
-- default configuration.
data Message t m a b = Message
  { _config :: MessageConfig t m a b
  }

instance (t ~ t', m ~ m') => UI t' m' (Message t m a b) where
  type Return t' m' (Message t m a b) = MessageResult t m a b
  ui' (Message config) = message config

dismissable :: MonadWidget t m => (Event t Bool)
            -> (a -> Event t ()) -> m a -> m (Dynamic t a)
dismissable hidden getDismiss m = do
  rec aDyn <- widgetHold m $ leftmost
        [ sample (current aDyn) <$ switch (current $ getDismiss <$> aDyn)
        , (\h -> if h then sample (current aDyn) else m) <$> hidden
        ]
  return aDyn

message
  :: forall t m a b. MonadWidget t m
  => MessageConfig t m a b
  -> m (Element EventResult (DomBuilderSpace m) t, MessageResult t m a b)
message config@MessageConfig {..} = do

  let dismissIcon = domEvent Click . fst <$> ui' (Icon "close" def)

  let content = do
        dismissed <- if _dismissable then dismissIcon else return never
        header <- traverse (divClass "header") _header
        message <- traverse (el "p") _message
        return (header, message, dismissed)

  rec
    hidden <- holdDyn False $ leftmost
      [ True <$ dismissed
      , _setHidden ]

    (divEl, (icon, (header, message, dismissed))) <-
      elActiveAttr' "div" (divAttrs $ Dynamic hidden) $ do
        case _icon of
          Nothing -> (,) Nothing <$> content
          Just icon -> do
            i <- ui' icon
            c <- divClass "content" content
            return (Just i, c)

  return $ (divEl, MessageResult
    { _header = header
    , _message = message
    , _icon = icon
    })

  where
    divAttrs hidden = mkDivAttrs <$> messageConfigClasses config <*> hidden
    mkDivAttrs c h = "class" =: getClass ("ui message" <> c <> memptyUnless "hidden" h)
