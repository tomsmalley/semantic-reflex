{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE LambdaCase           #-}

module Example.Section.Dropdown where

import Control.Lens
import Control.Monad ((<=<), void, join)
import Data.Foldable (for_)
import Data.Text (Text)
import Reflex.Dom.SemanticUI
import Reflex.Dom.Core (text)
import Data.Map.Lazy (Map)
import Data.Semigroup ((<>))

import Example.QQ
import Example.Common
import Example.StateEnum
import Example.CountryEnum

import qualified Data.Map.Lazy as M
import qualified Data.Text as T

dropdowns :: MonadWidget t m => Section t m
dropdowns = Section "Dropdown" (simpleLink "https://semantic-ui.com/modules/dropdown.html") $ do

  hscode $(printDefinition id stripParens ''DropdownConfig)

  pageHeader H3 def $ text "Dropdown"

  mkExample "Static or Dynamic Dropdown" (def
    & dynamic ?~ dynCode
    & subtitle ?~ text "A dropdown with lots of items. Note that the dynamic list takes a few times longer to load than the static list.")
    [resetExample|
  \resetEvent -> do
    on <- toggle True <=< button def $ text "Toggle"

    let items = [1 .. 1000] :: [Int]
        render = M.fromList [ (i, text $ tshow i) | i <- items ]

    e <- dyn $ ffor on $ \o -> case o of
      True -> dropdown (def & dropdownConfig_placeholder |~ "Static") Nothing $
        TaggedStatic render
      False -> dropdown (def & dropdownConfig_placeholder |~ "Dynamic") Nothing $
        TaggedDynamic $ pure render
    join <$> holdDyn (pure Nothing) (_dropdown_value <$> e)
  |]

  mkExample "Dropdown" (def
    & dynamic ?~ dynCode
    & subtitle ?~ text "A standard dropdown")
    [resetExample|
  \resetEvent -> do
    d <- dropdown def (Just 1) $ TaggedStatic $ 1 =: (text "one") <> 2 =: (text "two")
    return $ d ^. dropdown_value
  |]

  mkExample "Dropdown" (def
    & dynamic ?~ dynCode
    & subtitle ?~ text "A standard dropdown")
    [resetExample|
  \resetEvent -> do
    d <- dropdown def (Identity 1) $ TaggedStatic $ 1 =: (text "one") <> 2 =: (text "two")
    return $ d ^. dropdown_value
  |]

  mkExample "Dropdown" (def
    & dynamic ?~ dynCode
    & subtitle ?~ text "A standard dropdown")
    [resetExample|
  \resetEvent -> do
    d <- dropdown def [2] $ TaggedStatic $ 1 =: (text "one") <> 2 =: (text "two")
    return $ d ^. dropdown_value
  |]

{-
  ui_ $ Example "Dropdown" (def & subtitle ?~ text "A simple dropdown" & dynamic ?~ dynCode)
    [resetExample|
--    ui $ Header (def & icon ?~ Icon "tag" def) $ text "Filter by tag"
--    ui $ Divider def
--    ui $ MenuItem "important" def $ text "Important"
--    ui $ MenuItem "announcement" def $ text "Announcement"
--    ui_ $ MenuItem "discussion" def $ text "Discussion"
--    void $ dyn $ ffor togg $ \case
--      True -> ui $ MenuItem "A" def $ text "A"
--      False -> ui $ MenuItem "B" def $ text "B"
  |]
-}

--  ui_ $ Divider def
--
--  ddval <- ui $ MenuDropdown (mkDropdownConfig Nothing & selection |~ True) $
--    for_ [1..100] $ \i -> ui $ MenuItem i def $ text $ TaggedStatic $ tshow i
--
--  ui_ $ Divider $ def & hidden |~ True
--
--  display ddval
--
--  ui_ $ Divider def

--  ddval <- ui $ SelectionDropdown (mkDropdownConfig Nothing & selection |~ True) (return ()) $
--    TaggedStatic $ for [1..100] $ \i -> simpleItem i

  return ()
{-
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

-}
