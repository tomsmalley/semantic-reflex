{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Example.Common where

import Control.Lens
import Control.Monad ((<=<), void, when)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.SemanticUI

import Example.QQ

data Section m = LinkedSection Text (Restrict Inline m ()) (Restrict None m ())

removableWidget :: MonadWidget t m => Event t () -> m (Event t ()) -> m ()
removableWidget restore widget = do
  rec res <- widgetHold widget $ leftmost
        [ never <$ blank <$ switch (current res)
        , widget <$ restore
        ]
  return ()

dynShowCode :: (MonadWidget t m, DynShow t a) => a -> Restrict r m ()
dynShowCode a = do
  a' <- dynShow a
  Restrict $ void $ dyn $ runRestricted . hscode <$> a'

dynCode :: (MonadWidget t m, Show a) => Dynamic t a -> Restrict r m ()
dynCode d = Restrict $ void $ dyn $ runRestricted . hscode . show <$> d

simpleLink :: MonadWidget t m => Text -> Restrict Inline m ()
simpleLink url = ui_ $ Anchor (text $ Static url) $ def
  & href |?~ url

orElse :: a -> a -> Bool -> a
orElse a _ True = a
orElse _ b False = b

exampleCard :: forall t m a. MonadWidget t m => Text -> Text -> (String, Restrict None m a) -> Restrict None m a
exampleCard headerText subText (code, widget) = divClass "example" $ do
  -- Title segment
  ui $ Segment (def & attached |?~ TopAttached) $ do
    ui $ PageHeader H4 def $ ui $ Content def $ do
      text $ Static headerText
      when (subText /= "") $ ui $ SubHeader $ text $ Static subText
  -- Main segment
  widgetResult <- ui $ Segment (def & attached |?~ Attached) widget
  -- Control buttons
  rec codeIsOpen <- toggle False <=< ui $ Button
          (Dynamic $ "Hide Code" `orElse` "Show Code" <$> codeIsOpen)
          $ def -- & icon .~ AlwaysRender (Icon "code" def)
                & size |?~ Tiny
                & compact |~ True
                & attached |?~ Vertically BottomAttached
                & realButton .~ False -- needed for bottom attached to work

  void $ codeEl $ updated codeIsOpen
  return widgetResult
  where
    codeEl evt = elWithAnim "div"
      ( def
        & elConfigClasses |~ "ui fluid bottom center popup"
        & elConfigStyle |~ Style ("top" =: "auto")
        & elConfigTransition . event ?~ fmap mkTransition evt
      ) $ hscode code
    mkTransition t = Transition Scale $ def
      & direction ?~ (if t then In else Out)
      & duration .~ 0.3
      & forceVisible .~ True

exampleCardReset :: forall t m a. MonadWidget t m => Text -> Text -> (String, Event t () -> Restrict None m a) -> Restrict None m a
exampleCardReset headerText subText (code, widget) = divClass "example" $ do
  -- Title segment
  ui $ Segment (def & attached |?~ TopAttached) $ do
    ui $ PageHeader H4 def $ ui $ Content def $ do
      text $ Static headerText
      when (subText /= "") $ ui $ SubHeader $ text $ Static subText
  rec
    -- Main segment
    widgetResult <- ui $ Segment (def & attached |?~ Attached) $ widget resetEvent
    -- Control buttons
    (resetEvent, codeIsOpen) <- ui $ Buttons (def
      & size |?~ Tiny & attached |?~ BottomAttached) $ do

      r <- ui $ Button "Reset" $ def -- & icon .~ AlwaysRender (Icon "refresh" def)
      rec c <- toggle False <=< ui $ Button
            (Dynamic $ "Hide Code" `orElse` "Show Code" <$> c)
            def
--            (def & icon .~ AlwaysRender (Icon "code" def))
      return (r, c)

  void $ codeEl $ updated codeIsOpen
  return widgetResult
  where
    codeEl evt = elWithAnim "div"
      ( def
        & elConfigClasses |~ "ui fluid bottom center popup"
        & elConfigStyle |~ Style ("top" =: "auto")
        & elConfigTransition . event ?~ fmap mkTransition evt
      ) $ hscode code
    mkTransition t = Transition Scale $ def
      & direction ?~ (if t then In else Out)
      & duration .~ 0.3
      & forceVisible .~ True


exampleCardDyn :: forall t m a. MonadWidget t m
               => (a -> Restrict None m ()) -> Text -> Text
               -> (String, Event t () -> Restrict None m a) -> Restrict None m ()
exampleCardDyn renderResult headerText subText (code, widget) = divClass "example" $ do
  -- Title segment
  ui $ Segment (def & attached |?~ TopAttached) $ do
    ui $ PageHeader H4 def $ ui $ Content def $ do
      text $ Static headerText
      when (subText /= "") $ ui $ SubHeader $ text $ Static subText
  rec
    -- Main segment
    widgetResult <- ui $ Segment (def & attached |?~ Attached) $ widget resetEvent
    -- Value segment
    ui $ Segment (def & attached |?~ Attached) $ do
      ui $ ContentHeader def $ ui $ Content def $ do
        text $ Static "Value"
      renderResult widgetResult
    -- Control buttons
    (resetEvent, codeIsOpen) <- ui $ Buttons (def
      & size |?~ Tiny & attached |?~ BottomAttached) $ do

      r <- ui $ Button "Reset" $ def -- & icon .~ AlwaysRender (Icon "refresh" def)
      rec c <- toggle False <=< ui $ Button
            (Dynamic $ "Hide Code" `orElse` "Show Code" <$> c)
            (def) --  & icon .~ AlwaysRender (Icon "code" def))
      return (r, c)

  void $ codeEl $ updated codeIsOpen
  where
    codeEl evt = elWithAnim "div"
      ( def
        & elConfigClasses |~ "ui fluid bottom center popup"
        & elConfigStyle |~ Style ("top" =: "auto")
        & elConfigTransition . event ?~ fmap mkTransition evt
      ) $ hscode code
    mkTransition t = Transition Scale $ def
      & direction ?~ (if t then In else Out)
      & duration .~ 0.3
      & forceVisible .~ True

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

