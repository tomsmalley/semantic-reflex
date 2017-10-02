{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures       #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-do-bind #-}

module Example (example) where

import GHC.Tuple
import Control.Lens
import Control.Monad ((<=<), void, when, forM_, join, replicateM_, guard)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.SemanticUI
import Language.Javascript.JSaddle hiding ((!!))

import Example.StateEnum
import Example.CountryEnum
import Example.QQ

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

data Section m = LinkedSection Text Text (m ())

checkboxes :: forall t m. MonadWidget t m => Section m
checkboxes = LinkedSection "Checkbox" "" $ do

  $(printDefinition stripParens ''Checkbox)
  $(printDefinition stripParens ''CheckboxConfig)

  divClass "ui two column stackable grid" $ do
    divClass "row" $ do

      divClass "column" $ do
        exampleCardDyn id "Checkbox" "Standard checkbox styles" $ [mkExample|
        \resetEvent -> do
          normal <- divClass "ui compact segment"
            $ ui $ Checkbox "Normal checkbox"
            $ def & setValue .~ (Unchecked <$ resetEvent)
          toggle <- divClass "ui compact segment"
            $ ui $ Checkbox "Toggle checkbox (checked by default)"
            $ def & types .~ [Toggle]
                  & setValue .~ (Checked <$ resetEvent)
                  & initialValue .~ Checked
          slider <- divClass "ui compact segment"
            $ ui $ Checkbox "Slider checkbox"
            $ def & types .~ [Slider]
                  & setValue .~ (Unchecked <$ resetEvent)
          return $ traverse (view value) [normal, toggle, slider]
        |]

      divClass "column" $ do
        exampleCardDyn id "Disabled checkbox" "Checkboxes can be enabled or disabled" [mkExample|
        \resetEvent -> do
          enable <- ui $ Button "Enable" $ def
            & attached |?~ Horizontally LeftAttached
          disable <- ui $ Button "Disable" $ def
            & attached |?~ Horizontally RightAttached
          let enableEvent = leftmost [Enabled <$ enable, Disabled <$ disable]
          normal <- divClass "ui segment"
            $ ui $ Checkbox "Initially disabled"
            $ def & setValue .~ (Unchecked <$ resetEvent)
                  & initialEnabled .~ Disabled
                  & setEnabled .~ leftmost [enableEvent, Disabled <$ resetEvent]
          toggle <- divClass "ui segment"
            $ ui $ Checkbox "Initially disabled (checked by default)"
            $ def & types .~ [Toggle]
                  & setValue .~ (Checked <$ resetEvent)
                  & initialEnabled .~ Disabled
                  & initialValue .~ Checked
                  & setEnabled .~ leftmost [enableEvent, Disabled <$ resetEvent]
          return $ traverse (\cb -> (,) <$> view value cb <*> view enabled cb) [normal, toggle]
        |]

    divClass "row" $ do

      divClass "column" $ do
        exampleCardDyn id "Checkbox states" "Checkboxes can be indeterminate" [mkExample|
        \resetEvent -> do
          indeterminateButton <- ui $ Button "Indeterminate" $ def
            & attached |?~ Horizontally LeftAttached
          determinateButton <- ui $ Button "Determinate" $ def
            & attached |?~ Horizontally RightAttached
          let indeterminateEvent = leftmost [Indeterminate <$ indeterminateButton, Determinate <$ determinateButton]
          cb <- divClass "ui compact segment"
            $ ui $ Checkbox "Indeterminate"
            $ def & types .~ []
                  & setValue .~ (Checked <$ resetEvent)
                  & initialIndeterminate .~ Indeterminate
                  & initialValue .~ Checked
                  & setIndeterminate .~ leftmost [indeterminateEvent, Indeterminate <$ resetEvent]
          return $ (,) <$> view value cb <*> view indeterminate cb
        |]

      divClass "column" $ do
        exampleCardDyn id "Fitted checkbox" "A fitted checkbox does not leave padding for a label" [mkExample|
        \resetEvent -> do
          normal <- divClass "ui compact segment"
            $ ui $ Checkbox ""
            $ def & types .~ [Fitted]
                  & setValue .~ (Unchecked <$ resetEvent)
          slider <- divClass "ui compact segment"
            $ ui $ Checkbox ""
            $ def & types .~ [Fitted, Toggle]
                  & setValue .~ (Unchecked <$ resetEvent)
          toggle <- divClass "ui compact segment"
            $ ui $ Checkbox ""
            $ def & types .~ [Fitted, Slider]
                  & setValue .~ (Unchecked <$ resetEvent)
          return $ traverse (view value) [normal, slider, toggle]
        |]

    return ()

dropdowns :: MonadWidget t m => Section m
dropdowns = LinkedSection "Dropdown" "" $ do

  elAttr "a" ("href" =: "https://semantic-ui.com/modules/dropdown.html") $ text "Semantic UI Docs"
  $(printDefinition stripParens ''DropdownConfig)
  $(printDefinition stripParens ''DropdownItem)
  $(printDefinition stripParens ''DropdownItemConfig)

  ui $ Header H3 (text "Dropdown") def
  $(printDefinition stripParens ''Dropdown)
  el "p" $ text "The standard dropdown returns a Maybe to signify the possibility of no selection. However, if you specify an initial value, the user will be unable to deselect it. In this case you can clear the value with 'setValue' by passing 'Nothing'."

  exampleCardDyn id "Single value" "" [mkExample|
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
        exampleCardDyn id "Single value" "" [mkExample|
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
        exampleCardDyn id "Single value, search" "" [mkExample|
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
        exampleCardDyn id "Single value inline menu" "A dropdown can be formatted to appear inline in other content" [mkExample|
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
        exampleCardDyn id "Single value inline menu" "A dropdown can be formatted to appear inline in other content" [mkExample|
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
            ) $ def & icon ?~ Icon "trophy" def
        |]

    divClass "row" $ do

      divClass "column" $ do
        exampleCardDyn id "Multi value" "" [mkExample|
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
        exampleCardDyn id "Multi value, full-text search" "" [mkExample|
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
        exampleCardDyn id "Multi value, limited " "" [mkExample|
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
        exampleCardDyn id "Multi value, search, hidden labels " "" [mkExample|
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

flags :: MonadWidget t m => Section m
flags = LinkedSection "Flag" "A flag is used to represent a political state" $ do

  el "p" $ do
    text "For available flag types, see "
    elAttr "a" ("href" =: "https://semantic-ui.com/elements/flag.html")
      $ text "the Semantic UI docs"
    text "."

  $(printDefinition id ''Flag)

  exampleCard "Flag" "" [mkExample|
  mapM_ (ui . Flag . Static . T.toLower . T.pack . show) [minBound .. maxBound :: CountryEnum]
  |]

data Favourite
  = Haskell
  | Semantic
  | Reflex
  deriving (Eq, Show)

menu :: forall t m. MonadWidget t m => Section m
menu = LinkedSection "Menu" "A menu displays grouped navigation actions" $ do

  el "p" $ text "In Semantic UI menus are just exposed as styling elements and any active state must be managed by you. Here the current state is managed for you, providing a standalone widget which returns the currently selected value and the results of any sub widgets in a 'HList'."

  $(printDefinition stripParens ''Menu)
  $(printDefinition stripParens ''MenuDef)
  $(printDefinition id ''MenuItems)

  $(printDefinition stripParens ''MenuConfig)
  $(printDefinition stripParens ''MenuItemConfig)

  exampleCardDyn id "Header" "A menu item may include a header or may itself be a header" [mkExample|
  \resetEvent -> do
    (selected, HNil) <- ui $ Menu
      ( SubMenu (constDyn $ part $ Header H3 (text "Products") $ def & header .~ ContentHeader)
        ( MenuItem "Enterprise" "Enterprise" def
        $ MenuItem "Consumer" "Consumer" def
        $ MenuBase )
      $ SubMenu (constDyn $ part $ Header H3 (text "CMS Solutions") $ def & header .~ ContentHeader)
        ( MenuItem "Rails" "Rails" def
        $ MenuItem "Python" "Python" def
        $ MenuItem "PHP" "PHP" def
        $ MenuBase )
      $ SubMenu (constDyn $ part $ Header H3 (text "Hosting") $ def & header .~ ContentHeader)
        ( MenuItem "Shared" "Shared" def
        $ MenuItem "Dedicated" "Dedicated" def
        $ MenuBase )
      $ SubMenu (constDyn $ part $ Header H3 (text "Support") $ def & header .~ ContentHeader)
        ( MenuItem "E-mail Support" "E-mail Support" def
        $ MenuItem "FAQs" "FAQs" def
        $ MenuBase )
      $ MenuBase )
      $ def & vertical .~ True & setValue .~ (Nothing <$ resetEvent)
    return (selected :: Dynamic t (Maybe Text))
   |]

  exampleCardDyn id "Sub Menu" "A menu item may contain another menu nested inside that acts as a grouped sub-menu" [mkExample|
  \resetEvent -> do
    (selected, search `HCons` HNil) <- ui $ Menu
      ( MenuWidget ( do
        result <- ui $ Input $ def & placeholder |?~ "Search..."
        return $ result ^. value
        ) def
      $ SubMenu (constDyn $ text "Home")
        ( MenuItem "Search" "Search" def
        $ MenuItem "Add" "Add" def
        $ MenuItem "Remove" "Remove" def
        $ DropdownMenu "More"
          [ DropdownItem "Edit" "Edit" def
          , DropdownItem "Tag" "Tag" def
          ]
        $ MenuBase )
      $ MenuItem "Browse" "Browse" (def & icon ?~ Icon "grid layout" def)
      $ MenuItem "Messages" "Messages" def
      $ DropdownMenu "More"
        [ DropdownItem "Edit Profile" "Edit Profile" $ def & icon ?~ Icon "edit" def
        , DropdownItem "Choose Language" "Choose Language" $ def & icon ?~ Icon "globe" def
        , DropdownItem "Account Settings" "Account Settings" $ def & icon ?~ Icon "settings" def
        , Items "Even More"
          [ DropdownItem "Contact Us" "Contact Us" $ def & icon ?~ Icon "talk" def
          , DropdownItem "Make a Suggestion" "Make a Suggestion" $ def & icon ?~ Icon "idea" def
          ]
        ]
      $ MenuBase )
      $ def & vertical .~ True
            & setValue .~ (Nothing <$ resetEvent)
    return $ (,) <$> (selected :: Dynamic t (Maybe Text)) <*> search
   |]

  exampleCardDyn id "Arbitrary Widgets" "An item may contain an arbitrary widget with optional capture of the result" [mkExample|
  \resetEvent -> do
    let makeHeader doIcon txt = elAttr "div" ("style" =: "margin-bottom: 0.7em") $ do
          when doIcon $ void $ ui $ Icon "external" $ def & floated |?~ RightFloated
          elAttr "span" ("style" =: "font-weight: bold; text-size: 1.1em") $ text txt
        favs = map (\x -> DropdownItem (T.toLower x) x def) ["Haskell", "Semantic UI", "Reflex"]
    -- Type signature required or the value is ambiguous
    (selected :: Dynamic t (Maybe Int), fav `HCons` HNil) <- ui $ Menu
      ( MenuWidget_ ( do
          makeHeader True "Haskell"
          el "p" $ text "Purely functional programming language"
        ) (def & link .~ Link "http://haskell.org/")
      $ MenuWidget_ ( do
          makeHeader True "Semantic UI"
          el "p" $ text "UI development framework designed for theming"
        ) (def & link .~ Link "http://semantic-ui.com/")
      $ MenuWidget_ ( do
          makeHeader True "Reflex"
          el "p" $ text "Higher-order functional reactive programming"
        ) (def & link .~ Link "http://hackage.haskell.org/package/reflex")
      $ MenuWidget ( do
          makeHeader False "Favourite"
          ui $ Dropdown favs $ pure Nothing
            & placeholder .~ "Pick your favourite..."
            & selection .~ True
            & fluid .~ True
            & setValue .~ (Nothing <$ resetEvent)
        ) (def & link .~ NoLink)
      $ MenuBase )
      $ def & vertical .~ True
    return $ (,) <$> selected <*> fav
   |]

  exampleCardDyn id "Secondary Menu" "A menu can adjust its appearance to de-emphasize its contents" [mkExample|
  \resetEvent -> do
    (selected, search `HCons` _) <- ui $ MenuDef
      ( MenuItem "Home" "Home" def
      $ MenuItem "Messages" "Messages" def
      $ MenuItem "Friends" "Friends" def
      $ RightMenu
        ( MenuWidget (ui $ Input def) (def & link .~ NoLink)
        $ MenuItem "Logout" "Logout" def
        $ MenuBase )
      $ MenuBase
      ) $ pure "Home"
        & customMenu ?~ "secondary" & setValue .~ ("Home" <$ resetEvent)
    return $ (,) <$> (selected :: Dynamic t Text) <*> search ^. value
  |]

  exampleCardDyn id "Secondary Menu" "A menu can adjust its appearance to de-emphasize its contents" [mkExample|
  \resetEvent -> do
    (selected, search `HCons` _) <- ui $ Menu
      ( MenuItem "Home" "Home" def
      $ MenuItem "Messages" "Messages" def
      $ MenuItem "Friends" "Friends" def
      $ RightMenu
        ( MenuWidget (divClass "item" $ ui $ Input def) def
        $ MenuItem "Logout" "Logout" def
        $ MenuBase)
      $ MenuBase
      ) $ def & customMenu ?~ "secondary" & setValue .~ (Nothing <$ resetEvent)
    return $ (,) <$> (selected :: Dynamic t (Maybe Text)) <*> search ^. value
  |]

--  exampleCardDyn id "Vertical Menu" "A vertical menu displays elements vertically" [mkExample|
--  \resetEvent -> do
  resetEvent <- ui $ Button "ser" def
  do
    let counter txt = let widget = count <=< ui $ Button (Static txt) def :: m (Dynamic t Int)
                      in join <$> widgetHold widget (widget <$ resetEvent)
    inboxCount <- counter "Add inbox item"
    spamCount <- counter "Add spam item"
    updatesCount <- counter "Add updates item"
    let mkLabel dNum conf = Label (T.pack . show <$> dNum) $ def
          & leftIcon .~ RenderWhen ((>=5) <$> dNum) (Icon "mail" def)
          & hidden .~ fmap (<= 0) dNum
          & conf
          & color %~ zipDynWith (\a b -> if a >= 10 then Just Red else b) dNum
    (selected, search `HCons` HNil) <- ui $ Menu
      ( MenuItem ("inbox" :: Text) "Inbox"
          (def & color ?~ Teal
               & label ?~ mkLabel inboxCount
                  (\x -> x & color .~ pure (Just Teal) & pointing .~ pure (Just LeftPointing)))
      $ MenuItem "spam" "Spam" (def & label ?~ mkLabel spamCount id)
      $ MenuItem "updates" "Updates"
          (def & label ?~ mkLabel updatesCount (\x -> x & basic .~ pure True & color .~ pure (Just Black) & leftIcon .~ AlwaysRender (Icon "announcement" def)))
      $ MenuWidget (ui (Input $ def
        & icon .~ AlwaysRender (Icon "search" def)
        & placeholder |?~ "Search mail...")) def
      $ MenuBase
      ) $ def & setValue .~ (Just "Inbox" <$ resetEvent)
              & initialValue ?~ "Inbox"
              & vertical .~ True
    return $ (,) <$> selected <*> search ^. value
--  |]

  rec
    (eLabel, LabelResult {..}) <- ui' $ Label "test" $ def
      & leftIcon .~ RenderWhen ((>3) <$> labelClicked) (Icon "user" $ def & size |?~ Huge)
      & link .~ True
    labelClicked <- count $ domEvent Click eLabel
    iconClicked <- count . switch . current $ fmap (maybe never $ domEvent Click . fst) _leftIcon

  el "p" $ do
    text "The label has been clicked: "
    display labelClicked
    text " times."
  el "p" $ do
    text "The icon has been clicked: "
    display iconClicked
    text " times."

  return ()

radioGroups :: forall t m. MonadWidget t m => Section m
radioGroups = LinkedSection "Radio Group" "" $ do

  $(printDefinition stripParens ''RadioGroup)
  $(printDefinition stripParens ''RadioGroupConfig)
  $(printDefinition stripParens ''RadioItem)
  $(printDefinition stripParens ''RadioItemConfig)

  divClass "ui two column stackable grid" $ do
    divClass "row" $ do

      divClass "column" $ do
        exampleCardDyn id "Radio group" "" [mkExample|
        \resetEvent -> do
          let mkRadioItem x = RadioItem x (showFreq x) def
              freqencies = map mkRadioItem [minBound..maxBound]
          divClass "ui form" $ ui $ RadioGroup "frequency" freqencies $
            def & setValue .~ (Nothing <$ resetEvent)
        |]

      divClass "column" $ do
        exampleCardDyn id "Slider group" "" [mkExample|
        \resetEvent -> do
          let mkRadioItem x = RadioItem x (showThroughput x) def
              throughputs = mkRadioItem <$> [Metered 20, Metered 10, Metered 5, Unmetered]
          divClass "ui form" $ ui $ RadioGroup "throughput" throughputs $
            def & initialValue ?~ Unmetered
                & setValue .~ (Just Unmetered <$ resetEvent)
                & types .~ [Slider]
        |]

  return ()

labels :: forall t m. MonadWidget t m => Section m
labels = LinkedSection "Label" "" $ do

  $(printDefinition stripParens ''Label)
  $(printDefinition stripParens ''LabelConfig)

  ui $ Header H3 (text "Types") def

  divClass "ui equal width stackable grid" $ do

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Label" "" [mkExample|
        ui $ Label "Veronika" $ def
          & image .~ AlwaysRender (Image "https://semantic-ui.com/images/avatar/small/veronika.jpg" def)
          & detail |?~ "Friend"
          & color |?~ Blue
        ui $ Label "Jenny" $ def
          & image .~ AlwaysRender (Image "https://semantic-ui.com/images/avatar/small/jenny.jpg" def)
          & detail |?~ "Student"
          & color |?~ Teal
        ui $ Label "Christian" $ def
          & image .~ AlwaysRender (Image "https://semantic-ui.com/images/avatar/small/christian.jpg" def)
          & detail |?~ "Co-worker"
          & color |?~ Yellow
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCardReset "Label" "" [mkExample|
        \resetEvent -> do
          let src person = "https://semantic-ui.com/images/avatar/small/" <> person <> ".jpg"
              mkLabel (person, url, colour) = do
                LabelResult _ iconResult _ <- ui $ Label (pure person) $ def
                  & image .~ AlwaysRender (Image (pure $ src url) def)
                  & rightIcon .~ AlwaysRender (Icon "delete" def)
                  & color |?~ colour
                return $ switch . current $ fmap (maybe never (domEvent Click . fst)) iconResult
          mapM_ (removableWidget resetEvent . mkLabel)
            [ ("Justen", "justen", Grey)
            , ("Adrienne", "ade", Blue)
            , ("Zoe", "zoe", Teal)
            , ("Elliot", "elliot", Olive)
            , ("Joe", "joe", Yellow)
            , ("Matt", "matt", Orange)
            ]
        |]

  return ()

removableWidget :: MonadWidget t m => Event t () -> m (Event t ()) -> m ()
removableWidget restore widget = do
  rec res <- widgetHold widget $ leftmost
        [ never <$ blank <$ switch (current res)
        , widget <$ restore
        ]
  return ()

icons :: MonadWidget t m => Section m
icons = LinkedSection "Icon" "" $ do

  $(printDefinition stripParens ''Icon)
  $(printDefinition stripParens ''IconConfig)
  $(printDefinition stripParens ''IconsConfig)

  ui $ Header H3 (text "Groups") def

  divClass "ui equal width stackable grid" $ do

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Icons" "Several icons can be used together as a group" [mkExample|
        ui $ Icons
          [ Icon "circle" $ def & size |?~ Big & color |?~ Blue
          , Icon "car" $ def & inverted |~ True
          ] $ def & size ?~ Huge
        ui $ Icons
          [ Icon "thin circle" $ def & size |?~ Big
          , Icon "user" def
          ] $ def & size ?~ Huge
        ui $ Icons
          [ Icon "certificate" $ def
              & size |?~ Big & loading |~ True
              & color |?~ Grey & inverted |~ True
          , Icon "cloud download" def
          ] $ def & size ?~ Huge
        |]

      divClass "column" $ do
        exampleCard "Corner Icon" "A group of icons can display a smaller corner icon"
          [mkExample|
        ui $ Header H2 (text "Add on Twitter") $ def
          & icon ?~ Icons
              [ Icon "twitter" def
              , Icon "corner add" $ def & inverted |~ True
              ] (def & size ?~ Large)
        |]

orElse :: a -> a -> Bool -> a
orElse a _ True = a
orElse _ b False = b

exampleCard :: forall t m a. MonadWidget t m => Text -> Text -> (String, m a) -> m a
exampleCard headerText subText (code, widget) = divClass "ui segments" $ do
  -- Title segment
  divClass "ui segment" $ ui $ Header H4 (text headerText) $ def
      & subHeader .~ if subText == "" then Nothing else Just $ text subText
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
      & subHeader .~ if subText == "" then Nothing else Just $ text subText
  rec
    -- Main segment
    widgetResult <- divClass "ui segment" $ widget resetEvent
    -- Control buttons
    let attrs open = "class" =: ("ui basic tiny compact buttons"
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

exampleCardDyn :: forall t m a b. (Show a, MonadWidget t m)
               => (b -> Dynamic t a) -> Text -> Text
               -> (String, Event t () -> m b) -> m (Dynamic t a)
exampleCardDyn getDyn headerText subText (code, widget) = divClass "ui segments" $ do
  -- Title segment
  divClass "ui segment" $ ui $ Header H4 (text headerText) $ def
      & subHeader .~ if subText == "" then Nothing else Just $ text subText
  rec
    -- Main segment
    widgetResult <- fmap getDyn $ divClass "ui segment" $ widget resetEvent
    -- Control buttons
    let attrs open = "class" =: ("ui basic tiny compact buttons"
            <> if open then " attached" else " bottom attached")
    (resetEvent, codeIsOpen) <- elDynAttr "div" (attrs <$> codeIsOpen) $ do
      r <- ui $ Button "Reset" $ def & icon .~ AlwaysRender (Icon "refresh" def)
      rec c <- toggle False <=< ui $ Button
            (Dynamic $ "Hide Code" `orElse` "Show Code" <$> c)
            (def & icon .~ AlwaysRender (Icon "code" def))
      return (r, c)
  divClass "ui segment" $ do
    ui $ Header H4 (text "Value") $ def & header .~ ContentHeader
    dyn $ hscode . show <$> widgetResult
  void $ dyn $ codeEl <$> codeIsOpen
  return widgetResult
  where
    codeEl False = blank
    codeEl True = divClass "ui segment" $ hscode code

scrollIntoView :: Text -> JSM ()
scrollIntoView id = do
  document <- jsg ("document" :: Text)
  o <- obj
  o <# ("block" :: Text) $ ("start" :: Text)
  o <# ("behavior" :: Text) $ ("smooth" :: Text)
  mEl :: Maybe JSVal <- fromJSVal =<< document ^. js1 ("getElementById" :: Text) id
  case mEl of
    Nothing -> consoleLog ("el does not exist" :: Text) >> return ()
    Just el -> void $ el ^. js1 ("scrollIntoView" :: Text) o

getLocationHash :: JSM (Maybe Text)
getLocationHash = do
  document <- jsg ("document" :: Text)
  mhash :: Maybe Text <- fromJSVal =<< document ^. js ("location" :: Text) ^. js ("hash" :: Text)
  return $ T.stripPrefix "#" =<< mhash

setLocationHash :: Text -> JSM ()
setLocationHash hash = do
  history <- jsg ("history" :: Text)
  void $ history ^. js3 ("pushState" :: Text) jsNull jsNull hash'
  where
    hash' = if "#" `T.isPrefixOf` hash then hash else "#" <> hash


putSections :: MonadWidget t m => [Section m] -> m ()
putSections sections = do
  pb :: Event t () <- delay 0.1 =<< getPostBuild
  onLoadEvent <- performEvent $ liftJSM getLocationHash <$ pb
  performEvent_ $ liftJSM . scrollIntoView <$> fmapMaybe id onLoadEvent

  elAttr "div" ("id" =: "main" <> "class" =: "ui container") $ do
    -- Menu
    (stickyEl, _) <- divClass "ui dividing right rail" $ do
      elAttr' "div" ("class" =: "ui sticky") $ do

        ui $ Header H4 (text "Components") def
        --divClass "ui vertical following fluid accordion text menu" $ do
        (selected, HNil) <- ui $ Menu
          (renderItems sections) $ def
            & vertical .~ True
            & fluid .~ True
            & textContent .~ True
            & setValue .~ onLoadEvent
        performEvent_ $ fmap (\id -> do
          liftJSM $ setLocationHash id
          liftJSM $ scrollIntoView id
          ) $ fmapMaybe id $ updated selected

    -- Sections
    (contextEl, _) <- el' "div" $ do

      divClass "intro" $ do
        ui $ Header H2 (text "Introduction") def
        el "p" $ do
          text "This library aims to provide a type safe Haskell wrapper around Semantic UI components, to allow easy construction of nice looking web applications in GHCJS. It is currently in early development and started as a fork of the "
          elAttr "a" ("href" =: "https://github.com/reflex-frp/reflex-dom-semui") $ text "reflex-dom-semui"
          text " library."
        el "p" $ text "This page serves to provide an example of the library and components in use. Examples are shown along with the code that generated them."

        ui $ Header H3 (text "Overview") def
        el "p" $ text "The library exposes components in the form of data types. The convention is to have a record with all parts required to specify a component, with the last being a config type that contains the optional or unnecessary parts. All of the component types have overloaded field lenses so they can be modified concisely."
        el "p" $ do
          text "Components can be rendered using the function "
          hsCodeInline "ui :: (MonadWidget t m, UI t m a) => a -> m (Return t m a)"
          text "."

      forM_ sections $ \(LinkedSection heading subHeading child) -> do
        ui $ Header H2 (text heading) $ def
          & dividing .~ True
          & attributes .~ "id" =: toId heading <> "style" =: "margin-top: 3em"
          & subHeader .~ if subHeading == "" then Nothing else Just (text subHeading)
        child

    performEvent_ $ (void . liftJSM $ do
      o <- obj
      o <# ("offset" :: Text) $ (30 :: Int)
      o <# ("context" :: Text) $ _element_raw contextEl
      o <# ("observeChanges" :: Text) $ True
      jQuery (_element_raw stickyEl) ^. js1 ("sticky" :: Text) o) <$ pb

    return ()

  where
    toId = T.intercalate "-" . T.words . T.toLower
    renderItems [] = MenuBase
    renderItems (LinkedSection heading _ _:rest)
      = MenuItem (toId heading) heading def $ renderItems rest

example :: JSM ()
example = mainWidget $ do

  elAttr "div" ("id" =: "masthead" <> "class" =: "ui vertical segment") $ do
    divClass "ui container" $ do
      let semanticLogo = Image "https://semantic-ui.com/images/logo.png" $ def
            & size |?~ Massive & rounded |?~ Rounded
      ui $ Header H1 (text "Semantic UI for Reflex Dom") $ def
        & image ?~ semanticLogo
        & subHeader ?~ text "Documentation and examples"
      elAttr "a" ("class" =: "ui disabled button" <> "href" =: "") $ text "Hackage"
      elAttr "a" ("class" =: "ui teal button"
               <> "href" =: "https://github.com/tomsmalley/reflex-dom-semui") $ do
        ui $ Icon "github" $ def
        text "GitHub"
      return ()

  putSections [ menu, checkboxes, dropdowns, radioGroups, labels, icons, flags ]

