{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Example.Common where

import Control.Lens
import Control.Monad ((<=<), void)
import Data.Bool (bool)
import Data.Default
import Data.Foldable (traverse_, for_)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.SemanticUI

import Example.QQ

data Section t m = LinkedSection Text (Component Inline m ()) (Component None m ())

removableWidget :: MonadWidget t m => Event t () -> m (Event t ()) -> m ()
removableWidget restore widget = do
  rec res <- widgetHold widget $ leftmost
        [ never <$ blank <$ switch (current res)
        , widget <$ restore
        ]
  return ()

dynShowCode :: (MonadWidget t m, DynShow t a) => a -> Component r m ()
dynShowCode a = do
  a' <- dynShow a
  Component $ void $ dyn $ runComponent . hscode <$> a'

dynCode :: (MonadWidget t m, Show a) => Dynamic t a -> Component r m ()
dynCode d = Component $ void $ dyn $ runComponent . hscode . show <$> d

simpleLink :: MonadWidget t m => Text -> Component Inline m ()
simpleLink url = ui_ $ Anchor (text $ Static url) $ def
  & href |?~ url

orElse :: a -> a -> Bool -> a
orElse a _ True = a
orElse _ b False = b

data ExampleConf t m a = ExampleConf
  { _subtitle :: Maybe (Component Inline m ())
  , _inbetween :: Maybe (Component None m ())
  , _dynamic :: Maybe (a -> Component None m ())
  }
$(makeLenses ''ExampleConf)

instance Applicative m => Default (ExampleConf t m a) where
  def = ExampleConf
    { _subtitle = Nothing
    , _inbetween = Nothing
    , _dynamic = Nothing
    }

upstreamIssue :: MonadWidget t m => Int -> Text -> Component None m ()
upstreamIssue issue msg = ui_ $ Message def $ paragraph $ do
  ui $ Icon "warning sign" def
  text $ Static msg
  ui $ Anchor (ui $ Image (Static shield) $ def & floated |?~ RightFloated)
    $ def & href |?~ url
  where
    shield = "https://img.shields.io/github/issues/detail/s/"
          <> "Semantic-Org/Semantic-UI/" <> tshow issue <> ".svg?maxAge=2592000"
    url = "https://github.com/Semantic-Org/Semantic-UI/issues/" <> tshow issue

data Example t m a = Example
  { _name :: Text
  , _config :: ExampleConf t m a
  , _codeWidget :: (String, Either (Component None m a) (Event t () -> Component None m a))
  }

instance (t' ~ t, m' ~ m) => UI t' m' None (Example t m a) where
  type Return t' m' (Example t m a) = a
  ui' (Example name ExampleConf {..} (code, eitherWidget))

    = let exampleConf = def & padded |~ True & vertical |~ True
                            & classes |~ "example"

      in ui' $ Segment exampleConf $ do

    -- Title
    ui $ Header (def & floated |?~ LeftFloated) $ do
      text $ Static name
      traverse_ (ui_ . SubHeader) _subtitle

    -- Control buttons
    let bConf = def & floated |?~ RightFloated & size |?~ Large & basic |~ True
                    & icon |~ True & circular |~ True
    codeIsOpen <- toggle False <=< ui $ Button bConf $ ui $ Icon "code" def
    resetEvent <- case eitherWidget of
      Left _ -> return never
      Right _ -> ui $ Button bConf $ ui $ Icon "refresh" def

    ui $ Divider $ def & hidden |~ True & clearing |~ True

    for_ _inbetween $ \widget -> do
      widget
      ui $ Divider $ def & hidden |~ True & clearing |~ True

    let flexConfig a c = def
          & classes |~ addClass c "flex"
          & basic |~ True & clearing |~ True & padded .~ Dynamic codeIsOpen
          & attached .~ Dynamic (bool Nothing (Just a) <$> codeIsOpen)

    -- Widget segment
    widgetResult <- ui $ Segment (flexConfig TopAttached "widget") $
      case eitherWidget of
        Left widget -> widget
        Right widget -> widget resetEvent

    -- Value segment
    case _dynamic of
      Nothing -> blank
      Just f -> ui $ Segment (flexConfig Attached "value") $ f widgetResult

    -- Code segment
    let codeConfig evt = def
          & transition ?~ (def & event .~ fmap mkTransition evt
                               & initial .~ True)
          & attached |?~ BottomAttached
        mkTransition t = Transition Instant $ def
          & direction ?~ bool Out In t

    ui $ Segment (codeConfig $ updated codeIsOpen) $ hscode code

    return widgetResult

-- | Throughput
data Throughput = Unmetered | Metered Int deriving (Eq, Show)

showThroughput :: Throughput -> Text
showThroughput Unmetered = "Unmetered"
showThroughput (Metered n) = T.pack (show n) <> " mbps max"

-- | Frequency
data Frequency = OnceWeek | TwiceWeek | OnceDay | TwiceDay
  deriving (Eq, Show, Enum, Bounded)

showFreq :: Frequency -> Text
showFreq OnceWeek = "Once a week"
showFreq TwiceWeek = "2-3 times a week"
showFreq OnceDay = "Once a day"
showFreq TwiceDay = "Twice a day"

-- | Contacts
data ContactEnum
  = Jenny | Elliot | Stevie | Christian | Matt | Justen
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

showContact :: ContactEnum -> Text
showContact Jenny = "Jenny Hess"
showContact Elliot = "Elliot Fu"
showContact Stevie = "Stevie Feliciano"
showContact Christian = "Christian"
showContact Matt = "Matt"
showContact Justen = "Justen Kitsune"

-- | Cards
data CardEnum = Visa | Amex | Discover
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

showCard :: CardEnum -> Text
showCard Visa = "Visa"
showCard Amex = "American Express"
showCard Discover = "Discover"

