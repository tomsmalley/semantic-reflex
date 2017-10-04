{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Example.Common where

import Control.Lens
import Control.Monad ((<=<), void)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.SemanticUI

import Example.QQ

data Section m = LinkedSection Text (m ()) (m ())

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

simpleLink :: DomBuilder t m => Text -> m ()
simpleLink url = elAttr "a" ("href" =: url) $ text url

orElse :: a -> a -> Bool -> a
orElse a _ True = a
orElse _ b False = b

exampleCard :: forall t m a. MonadWidget t m => Text -> Text -> (String, m a) -> m a
exampleCard headerText subText (code, widget) = divClass "ui segments" $ do
  -- Title segment
  divClass "ui segment" $ ui $ Header H4 (text headerText) $ def
      & subHeader .~ if subText == "" then Nothing else Just (text subText)
  -- Main segment
  widgetResult <- divClass "ui segment" widget
  -- Control buttons
--  let classes open = def { _uiButton_custom = Just ("basic tiny compact"
--      <> if open then " attached" else " bottom attached") }
  let attrs open = "class" =: ("ui basic tiny compact buttons"
          <> if open then " attached" else " bottom attached")
  rec codeIsOpen <- toggle False <=< elDynAttr "div" (attrs <$> codeIsOpen)
        $ ui $ Button
          (Dynamic $ "Hide Code" `orElse` "Show Code" <$> codeIsOpen)
          (def & icon .~ AlwaysRender (Icon "code" def))
  void $ dyn $ codeEl <$> codeIsOpen
  return widgetResult
  where
    codeEl False = blank
    codeEl True = divClass "ui segment" $ hscode code

exampleCardReset :: forall t m a. MonadWidget t m => Text -> Text -> (String, Event t () -> m a) -> m a
exampleCardReset headerText subText (code, widget) = divClass "ui segments" $ do
  -- Title segment
  divClass "ui segment" $ ui $ Header H4 (text headerText) $ def
      & subHeader .~ if subText == "" then Nothing else Just (text subText)
  rec
    -- Main segment
    widgetResult <- divClass "ui segment" $ widget resetEvent
    -- Control buttons
    let attrs open = "class" =: ("ui two basic tiny compact buttons"
            <> if open then " attached" else " bottom attached")
    (resetEvent, codeIsOpen) <- elDynAttr "div" (attrs <$> codeIsOpen) $ do
      r <- ui $ Button "Reset" $ def & icon .~ AlwaysRender (Icon "refresh" def)
      rec c <- toggle False <=< ui $ Button
            (Dynamic $ "Hide Code" `orElse` "Show Code" <$> c)
            (def & icon .~ AlwaysRender (Icon "code" def))
      return (r, c)
  void $ dyn $ codeEl <$> codeIsOpen
  return widgetResult
  where
    codeEl False = blank
    codeEl True = divClass "ui segment" $ hscode code

exampleCardDyn :: forall t m a. MonadWidget t m
               => (a -> m ()) -> Text -> Text
               -> (String, Event t () -> m a) -> m ()
exampleCardDyn renderResult headerText subText (code, widget) = divClass "ui segments" $ do
  -- Title segment
  divClass "ui segment" $ ui $ Header H4 (text headerText) $ def
      & subHeader .~ if subText == "" then Nothing else Just (text subText)
  rec
    -- Main segment
    widgetResult <- divClass "ui segment" $ widget resetEvent
    -- Value segment
    divClass "ui segment" $ do
      ui $ Header H4 (text "Value") $ def & header .~ ContentHeader
      renderResult widgetResult
    -- Control buttons
    let attrs open = "class" =: ("ui two basic tiny compact buttons"
            <> if open then " attached" else " bottom attached")
    (resetEvent, codeIsOpen) <- elDynAttr "div" (attrs <$> codeIsOpen) $ do
      r <- ui $ Button "Reset" $ def & icon .~ AlwaysRender (Icon "refresh" def)
      rec c <- toggle False <=< ui $ Button
            (Dynamic $ "Hide Code" `orElse` "Show Code" <$> c)
            (def & icon .~ AlwaysRender (Icon "code" def))
      return (r, c)
  void $ dyn $ codeEl <$> codeIsOpen
  where
    codeEl False = blank
    codeEl True = divClass "ui segment" $ hscode code

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

