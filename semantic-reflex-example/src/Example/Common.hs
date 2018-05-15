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
import Control.Monad.Fix (MonadFix)
import Control.Monad ((<=<), void)
import Data.Bool (bool)
import Data.Default
import Data.Foldable (traverse_, for_)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.SemanticUI

import Example.QQ

data Section t m = Section
  { sectionHeader :: Text
  , sectionDetail :: m ()
  , sectionContent :: m ()
  }

removableWidget :: MonadWidget t m => Event t () -> m (Event t ()) -> m ()
removableWidget restore widget = do
  rec res <- widgetHold widget $ leftmost
        [ never <$ blank <$ switch (current res)
        , widget <$ restore
        ]
  return ()

data CountWithLast a = NotFired | Fired Int a deriving (Eq, Show)
countWithLast
  :: (Reflex t, MonadHold t m, MonadFix m)
  => Event t a -> m (Dynamic t (CountWithLast a))
countWithLast = holdDyn NotFired <=< zipListWithEvent Fired [1..]

class DynShow t a where
  dynShow :: (Reflex t, MonadHold t m, MonadFix m) => a -> m (Dynamic t String)

instance DynShow t (Checkbox t) where
  dynShow Checkbox {..} = do
    change <- countWithLast _checkbox_change
    pure $ mconcat
      [ pure "Checkbox"
      , (("\n  { _checkbox_value = " <>) . show) <$> _checkbox_value
      , (("\n  , _checkbox_change = " <>) . show) <$> change
      , (("\n  , _checkbox_indeterminate = " <>) . show)
        <$> _checkbox_indeterminate
      , (("\n  , _checkbox_hasFocus = " <>) . show) <$> _checkbox_hasFocus
      , pure "\n  }"
      ]

instance DynShow t (Progress t m) where
  dynShow Progress {..} = do
    pure $ mconcat
      [ pure "Progress"
      , (("\n  { _progress_percent = " <>) . show) <$> _progress_percent
      , pure "\n  }"
      ]

instance DynShow t (TextInput t) where
  dynShow TextInput {..} = do
    input <- countWithLast _textInput_input
    keypress <- countWithLast _textInput_keypress
    keydown <- countWithLast _textInput_keydown
    keyup <- countWithLast _textInput_keyup
    pure $ mconcat
      [ pure "TextInput"
      , (("\n  { _textInput_value = " <>) . show) <$> _textInput_value
      , (("\n  , _textInput_input = " <>) . show) <$> input
      , (("\n  , _textInput_keypress = " <>) . show) <$> keypress
      , (("\n  , _textInput_keydown = " <>) . show) <$> keydown
      , (("\n  , _textInput_keyup = " <>) . show) <$> keyup
      , (("\n  , _textInput_hasFocus = " <>) . show) <$> _textInput_hasFocus
      , pure "\n  }"
      ]

dynShowCode :: (MonadWidget t m, DynShow t a) => a -> m ()
dynShowCode a = do
  a' <- dynShow a
  void $ dyn $ hscode <$> a'

dynCode :: (MonadWidget t m, Show a) => Dynamic t a -> m ()
dynCode d = void $ dyn $ hscode . show <$> d

simpleLink :: MonadWidget t m => Text -> m ()
simpleLink url = void $ hyperlink url $ text url

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
  hyperlink url $ image (def & imageConfig_floated |?~ RightFloated) $ Left $ Img (pure shield) def
  where
    shield = "https://img.shields.io/github/issues/detail/s/"
          <> "Semantic-Org/Semantic-UI/" <> tshow issue <> ".svg?maxAge=2592000"
    url = "https://github.com/Semantic-Org/Semantic-UI/issues/" <> tshow issue

mkExample
  :: MonadWidget t m
  => Text -> ExampleConf t m a -> (String, Either (m a) (Event t () -> m a))
  -> m () -- (El t, a)
mkExample name ExampleConf {..} (code, eitherWidget)

  = let exampleConf = def & segmentConfig_padded |~ True & segmentConfig_vertical |~ True
                          & classes |~ "example"

    in void . segment exampleConf $ do

  -- Title
  header (def & headerConfig_floated |?~ LeftFloated) $ do
    text name
    traverse_ subHeader _subtitle

  -- Control buttons
  let bConf = def & buttonConfig_floated |?~ RightFloated
                  & buttonConfig_size |?~ Large
                  & buttonConfig_basic |~ True
                  & buttonConfig_icon |~ True
                  & buttonConfig_circular |~ True
  codeIsOpen <- toggle False <=< button bConf $ icon "code" def
  resetEvent <- case eitherWidget of
    Left _ -> return never
    Right _ -> button bConf $ icon "refresh" def

  divider $ def & dividerConfig_hidden |~ True & dividerConfig_clearing |~ True

  for_ _inbetween $ \widget -> do
    widget
    divider $ def & dividerConfig_hidden |~ True & dividerConfig_clearing |~ True

  let flexConfig a c = def
        & classes |~ addClass c "flex"
        & segmentConfig_basic |~ True
        & segmentConfig_clearing |~ True
        & segmentConfig_padded .~ Dyn codeIsOpen
        & segmentConfig_attached .~ Dyn (bool Nothing (Just a) <$> codeIsOpen)

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
        & action ?~ (def
          & action_event ?~ fmap mkTransition evt
          & action_initialDirection .~ Out)
        & segmentConfig_attached |?~ BottomAttached
      mkTransition t = Transition Instant $ def
        & transitionConfig_direction ?~ bool Out In t

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

