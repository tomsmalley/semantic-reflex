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
import Reflex.Dom.Core hiding (button)

import Example.QQ

data Section t m = LinkedSection Text (m ()) (m ())

removableWidget :: MonadWidget t m => Event t () -> m (Event t ()) -> m ()
removableWidget restore widget = do
  rec res <- widgetHold widget $ leftmost
        [ never <$ blank <$ switch (current res)
        , widget <$ restore
        ]
  return ()

dynShowCode :: (MonadWidget t m, DynShow t a) => a -> m ()
dynShowCode a = do
  a' <- dynShow a
  void $ dyn $ hscode <$> a'

dynCode :: (MonadWidget t m, Show a) => Dynamic t a -> m ()
dynCode d = void $ dyn $ hscode . show <$> d

simpleLink :: MonadWidget t m => Text -> m ()
simpleLink url = void $ hyperlink (pure $ pure url) (Static url)

orElse :: a -> a -> Bool -> a
orElse a _ True = a
orElse _ b False = b

data ExampleConf t m a = ExampleConf
  { _subtitle :: Maybe (m ())
  , _inbetween :: Maybe (m ())
  , _dynamic :: Maybe (a -> m ())
  }
$(makeLenses ''ExampleConf)

instance Applicative m => Default (ExampleConf t m a) where
  def = ExampleConf
    { _subtitle = Nothing
    , _inbetween = Nothing
    , _dynamic = Nothing
    }

upstreamIssue :: MonadWidget t m => Int -> Text -> m ()
upstreamIssue issue msg = message def $ paragraph $ do
  icon "warning sign" def
  text msg
  elAttr "a" ("href" =: url) $
    image (Static shield) $ def & imageFloated |?~ RightFloated
  where
    shield = "https://img.shields.io/github/issues/detail/s/"
          <> "Semantic-Org/Semantic-UI/" <> tshow issue <> ".svg?maxAge=2592000"
    url = "https://github.com/Semantic-Org/Semantic-UI/issues/" <> tshow issue

mkExample
  :: MonadWidget t m
  => Text -> ExampleConf t m a -> (String, Either (m a) (Event t () -> m a))
  -> m () -- (El t, a)
mkExample name ExampleConf {..} (code, eitherWidget)

  = let exampleConf = def & segmentPadded |~ True & segmentVertical |~ True
                          & classes |~ "example"

    in void . segment exampleConf $ do

  -- Title
  header (def & headerFloated |?~ LeftFloated) $ do
    text name
    traverse_ subHeader _subtitle

  -- Control buttons
  let bConf = def & buttonFloated |?~ RightFloated
                  & buttonSize |?~ Large
                  & buttonBasic |~ True
                  & buttonIcon |~ True
                  & buttonCircular |~ True
  codeIsOpen <- toggle False <=< button bConf $ icon "code" def
  resetEvent <- case eitherWidget of
    Left _ -> return never
    Right _ -> button bConf $ icon "refresh" def

  divider $ def & dividerHidden |~ True & dividerClearing |~ True

  for_ _inbetween $ \widget -> do
    widget
    divider $ def & dividerHidden |~ True & dividerClearing |~ True

  let flexConfig a c = def
        & classes |~ addClass c "flex"
        & segmentBasic |~ True
        & segmentClearing |~ True
        & segmentPadded .~ Dynamic codeIsOpen
        & segmentAttached .~ Dynamic (bool Nothing (Just a) <$> codeIsOpen)

  -- Widget segment
  widgetResult <- segment (flexConfig TopAttached "widget") $
    case eitherWidget of
      Left widget -> widget
      Right widget -> widget resetEvent

  -- Value segment
  case _dynamic of
    Nothing -> blank
    Just f -> segment (flexConfig Attached "value") $ f widgetResult

  -- Code segment
  let codeConfig evt = def
        & transition ?~ (def
          & transConfigEvent .~ fmap mkTransition evt
          & transConfigInitialDirection .~ Out)
        & segmentAttached |?~ BottomAttached
      mkTransition t = Transition Instant $ def
        & transitionDirection ?~ bool Out In t

  segment (codeConfig $ updated codeIsOpen) $ hscode code

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

