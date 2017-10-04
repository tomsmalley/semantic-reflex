{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}

module Example.Section.Dropdown where

import GHC.Tuple -- TH requires this for (,)
import Control.Lens
import Control.Monad ((<=<), void, when, join)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.SemanticUI

import Example.QQ
import Example.Common
import Example.StateEnum
import Example.CountryEnum

dropdowns :: MonadWidget t m => Section m
dropdowns = LinkedSection "Dropdown" blank $ do

  elAttr "a" ("href" =: "https://semantic-ui.com/modules/dropdown.html") $ text "Semantic UI Docs"
  hscode $ $(printDefinition id stripParens ''DropdownConfig)
  hscode $ $(printDefinition id stripParens ''DropdownItem)
  hscode $ $(printDefinition id stripParens ''DropdownItemConfig)

  ui $ Header H3 (text "Dropdown") def
  hscode $ $(printDefinition id stripParens ''Dropdown)
  el "p" $ text "The standard dropdown returns a Maybe to signify the possibility of no selection. However, if you specify an initial value, the user will be unable to deselect it. In this case you can clear the value with 'setValue' by passing 'Nothing'."

  exampleCardDyn dynCode "Single value" "" [mkExample|
  \resetEvent -> do
    ui $ Dropdown
      [ Content $ Header H3 (text "One Or Two") def
      , Content Divider
      , DropdownItem (1 :: Int) "One" def
      , DropdownItem 2 "Two" def
      , Content $ Header H2 (text "Greater Than Three") def
      , Content Divider
      , Items "More"
        [ DropdownItem 3 "Three" def
        , DropdownItem 4 "Four" def
        , DropdownItem 5 "Five" def
        ]
      , DropdownItem 6 "Six" def
      , DropdownItem 7 "Seven" def
      , DropdownItem 8 "Eight" def
      ] $ pure Nothing
        & placeholder .~ "Pick a number"
        & setValue .~ (Nothing <$ resetEvent)
  |]

  divClass "ui two column stackable grid" $ do
    divClass "row" $ do

      divClass "column" $ do
        exampleCardDyn dynCode "Single value" "" [mkExample|
        \resetEvent -> do
          clearEvent <- ui $ Button "Clear Value" $ def
            & attached |?~ Horizontally LeftAttached
          let mkItem card = DropdownItem card (showCard card) $ def
                & icon ?~ Icon (pure . T.toLower $ tshow card) def
              cards = map mkItem [minBound..maxBound]
          ui $ Dropdown cards
            $ def & placeholder .~ "Card Type"
                  & setValue .~ leftmost [Just Visa <$ resetEvent, Nothing <$ clearEvent]
                  & initialValue ?~ Visa
                  & selection .~ True
        |]

      divClass "column" $ do
        exampleCardDyn dynCode "Single value, search" "" [mkExample|
        \resetEvent -> do
          let mkItem contact = DropdownItem contact (showContact contact) $ def
                & image ?~ Image (src contact) (def & size |?~ Mini & avatar |~ True)
              src contact = pure $ "http://semantic-ui.com/images/avatar/small/"
                          <> T.toLower (tshow contact) <> ".jpg"
              contacts = map mkItem [minBound..maxBound]
          ui $ Dropdown contacts
            $ def & placeholder .~ "Saved Contacts"
                  & setValue .~ (Nothing <$ resetEvent)
                  & selection .~ True
                  & search .~ True
                  & textOnly .~ True
        |]

  el "p" $ text "Dropdown values can be definite: that is, they are guaranteed to have a value and cannot be deselected by the user."

  divClass "ui warning message" $ do
    ui $ Icon "warning sign" def
    text "If you fire a setValue event with a non-existant value, the event will be ignored."

  divClass "ui two column stackable grid" $ do
    divClass "row" $ do

      divClass "column" $ do
        exampleCardDyn dynCode "Single value inline menu" "A dropdown can be formatted to appear inline in other content" [mkExample|
        \resetEvent -> el "span" $ do
          let mkItem contact = DropdownItem contact (showContact contact) $ def
                & image ?~ Image (src contact) (def & avatar |~ True)
              src contact = pure $ "https://semantic-ui.com/images/avatar/small/"
                          <> T.toLower (tshow contact) <> ".jpg"
              contacts = map mkItem [minBound..maxBound]
          text $ "Show me posts by "
          ui $ Dropdown contacts
            $ pure (Identity Jenny)
                & inline .~ True
                & setValue .~ (Identity Jenny <$ resetEvent)
        |]

      divClass "column" $ do
        exampleCardDyn dynCode "Single value inline menu" "A dropdown can be formatted to appear inline in other content" [mkExample|
        \resetEvent -> do
          setEvent <- ui $ Button "Set Value Incorrectly" def
          ui $ Header H4 ( do
            text "Trending repos "
            ui $ Dropdown
              [ Content $ Header H1 (text "Adjust time span") def
              , Content Divider
              , DropdownItem "daily" "Today" $ def & dataText ?~ "today"
              , DropdownItem "weekly" "This Week" $ def & dataText ?~ "this week"
              , DropdownItem "monthly" "This Month" $ def & dataText ?~ "this month"
              ]
              $ pure (Identity ("daily" :: Text))
                  & inline .~ True
                  & setValue .~ leftmost
                    [ Identity "daily" <$ resetEvent
                    , Identity "error" <$ setEvent ]
            ) $ def & icon .~ AlwaysRender (Icon "trophy" def)
        |]

    divClass "row" $ do

      divClass "column" $ do
        exampleCardDyn dynCode "Multi value" "" [mkExample|
        \resetEvent -> do
          let mkItem card = DropdownItem card (showCard card) $ def
                & icon ?~ Icon (pure . T.toLower $ tshow card) def
              cards = map mkItem [minBound..maxBound]
          ui $ Dropdown cards
            $ def & placeholder .~ "Card Type"
                  & setValue .~ ([] <$ resetEvent)
                  & selection .~ True
                  & textOnly .~ True
        |]

      divClass "column" $ do
        exampleCardDyn dynCode "Multi value, full-text search" "" [mkExample|
        \resetEvent -> do
          let mkItem contact = DropdownItem contact (showContact contact) $ def
                & image ?~ Image (src contact) (def & size |?~ Mini & avatar |~ True)
                & dataText ?~ (T.unwords $ take 1 $ T.words $ showContact contact)
              src contact = pure $ "http://semantic-ui.com/images/avatar/small/"
                          <> T.toLower (tshow contact) <> ".jpg"
              contacts = map mkItem [minBound..maxBound]
          ui $ Dropdown contacts
            $ def & placeholder .~ "Saved Contacts"
                  & setValue .~ ([Matt, Elliot] <$ resetEvent)
                  & initialValue .~ [Matt, Elliot]
                  & fullTextSearch .~ True
                  & selection .~ True
                  & search .~ True
        |]

    divClass "row" $ do

      divClass "column" $ do
        exampleCardDyn dynCode "Multi value, limited " "" [mkExample|
        \resetEvent -> do
          let mkItem state = DropdownItem state (stateText state) $ def
              states = map mkItem [minBound..maxBound]
          ui $ Dropdown states
            $ def & placeholder .~ "States"
                  & setValue .~ ([] <$ resetEvent)
                  & maxSelections ?~ 3
                  & selection .~ True
        |]

      divClass "column" $ do
        exampleCardDyn dynCode "Multi value, search, hidden labels " "" [mkExample|
        \resetEvent -> do
          let mkItem country = DropdownItem country (countryText country) $ def
                & flag ?~ Flag (pure $ T.toLower $ T.pack $ show country)
              countries = map mkItem [minBound..maxBound]
          ui $ Dropdown countries
            $ def & placeholder .~ "Country"
                  & setValue .~ ([] <$ resetEvent)
                  & useLabels .~ False
                  & selection .~ True
                  & search .~ True
        |]

  return ()

