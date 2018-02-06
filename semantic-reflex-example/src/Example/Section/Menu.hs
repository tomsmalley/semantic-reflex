{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Example.Section.Menu where

import Control.Lens
import Control.Monad ((<=<), join)
import Data.List.NonEmpty
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.Text (Text)
import Reflex.Dom.SemanticUI
import Reflex.Dom.Core (text)
import GHC.Tuple

import Example.QQ
import Example.Common

data Favourite
  = Haskell
  | Semantic
  | Reflex
  deriving (Eq, Show)

menuSection :: forall t m. MonadWidget t m => Section t m
menuSection = Section "Menu" (text "A menu displays grouped navigation actions") $ do

  paragraph $ text "In Semantic UI menus are just exposed as styling elements and any active state must be managed by you. Here the state is managed for you."

--  hscode $(printDefinition id stripParens ''MenuDef)
--  hscode $(printDefinition id id ''MenuItems)

  hscode $(printDefinition id stripParens ''MenuConfig)
  hscode $(printDefinition id stripParens ''MenuItemConfig)

{-

  ui_ $ Example "Identity Menu" (def & subtitle ?~ text "A menu can have a single definite value" & dynamic ?~ dynCode)
   [resetExample|
  \resetEvent -> do

    let menuConfig = mkMenuConfig (Identity 1)
          & value . event ?~ (Identity 1 <$ resetEvent)

    (dynVal, _) <- ui $ Menu menuConfig $ do
      ui_ $ MenuItem 1 def $ text "One"
      ui_ $ MenuItem 2 def $ text "Two"
      ui_ $ MenuItem 3 def $ text "Three"
      ui_ $ MenuItem 4 def $ text "Four"

    return dynVal
  |]

  ui_ $ Example "Maybe Menu" (def & subtitle ?~ text "A menu can have a single deselectable value" & dynamic ?~ dynCode)
   [resetExample|
  \resetEvent -> do

    let menuConfig = def & value . event ?~ (Nothing <$ resetEvent)

    (dynVal, _) <- ui $ Menu menuConfig $ do
      ui_ $ MenuItem 1 def $ text "One"
      ui_ $ MenuItem 2 def $ text "Two"
      ui_ $ MenuItem 3 def $ text "Three"
      ui_ $ MenuItem 4 def $ text "Four"

    return dynVal
  |]

  ui_ $ Example "List Menu" (def
    & subtitle ?~ text "A menu can have many (zero or more) selected values"
    & inbetween ?~ ui_ (Message (def & messageType |?~ InfoMessage) $ paragraph $ do
        ui $ Icon "info" def
        text "The selections of List / NonEmpty menus are in order of addition, from oldest to newest.")
    & dynamic ?~ dynCode)
   [resetExample|
  \resetEvent -> do

    let menuConfig = def & value . event ?~ ([] <$ resetEvent)

    (dynVal, _) <- ui $ Menu menuConfig $ do
      ui_ $ MenuItem 1 def $ text "One"
      ui_ $ MenuItem 2 def $ text "Two"
      ui_ $ MenuItem 3 def $ text "Three"
      ui_ $ MenuItem 4 def $ text "Four"

    return dynVal
  |]

  ui_ $ Example "NonEmpty Menu" (def & subtitle ?~ text "A menu can have some (one or more) selected values" & dynamic ?~ dynCode)
   [resetExample|
  \resetEvent -> do

    let menuConfig = mkMenuConfig (3 :| [])  & value . event ?~ ((3 :| []) <$ resetEvent)

    (dynVal, _) <- ui $ Menu menuConfig $ do
      ui_ $ MenuItem 1 def $ text "One"
      ui_ $ MenuItem 2 def $ text "Two"
      ui_ $ MenuItem 3 def $ text "Three"
      ui_ $ MenuItem 4 def $ text "Four"

    return dynVal
  |]

  ui_ $ Example "Set Menu" (def & subtitle ?~ text "A menu can have many (zero or more) ordered selected values" & dynamic ?~ dynCode)
   [resetExample|
  \resetEvent -> do

    let menuConfig = mkMenuConfig (S.singleton 3) & value . event ?~ (S.singleton 3 <$ resetEvent)

    (dynVal, _) <- ui $ Menu menuConfig $ do
      ui_ $ MenuItem 1 def $ text "One"
      ui_ $ MenuItem 2 def $ text "Two"
      ui_ $ MenuItem 3 def $ text "Three"
      ui_ $ MenuItem 4 def $ text "Four"

    return dynVal
  |]

  ui_ $ Example "Seq Menu" (def & subtitle ?~ text "A menu can have many (zero or more) selected values as a sequence" & dynamic ?~ dynCode)
   [resetExample|
  \resetEvent -> do

    let menuConfig = mkMenuConfig (Seq.singleton 3) & value . event ?~ (Seq.singleton 3 <$ resetEvent)

    (dynVal, _) <- ui $ Menu menuConfig $ do
      ui_ $ MenuItem 1 def $ text "One"
      ui_ $ MenuItem 2 def $ text "Two"
      ui_ $ MenuItem 3 def $ text "Three"
      ui_ $ MenuItem 4 def $ text "Four"

    return dynVal
  |]

  ui_ $ Example "Limited Seq Menu" (def & subtitle ?~ text "A menu can have many (zero or more) selected values as a limited sequence" & dynamic ?~ dynCode)
   [resetExample|
  \resetEvent -> do

    let menuConfig = mkMenuConfig (LimitedSeq 2 False $ pure 3) & value . event ?~ (LimitedSeq 2 False (pure 3) <$ resetEvent)

    (dynVal, _) <- ui $ Menu menuConfig $ do
      ui_ $ MenuItem (1 :: Int) def $ text "One"
      ui_ $ MenuItem 2 def $ text "Two"
      ui_ $ MenuItem 3 def $ text "Three"
      ui_ $ MenuItem 4 def $ text "Four"

    return dynVal
  |]

  ui_ $ Example "Cycling Limited Seq Menu" (def & subtitle ?~ text "A menu can have many (zero or more) selected values as a cycling limited sequence" & dynamic ?~ dynCode)
   [resetExample|
  \resetEvent -> do

    let menuConfig = mkMenuConfig (LimitedSeq 2 True $ pure 3) & value . event ?~ (LimitedSeq 2 True (pure 3) <$ resetEvent)

    (dynVal, _) <- ui $ Menu menuConfig $ do
      ui_ $ MenuItem (1 :: Int) def $ text "One"
      ui_ $ MenuItem 2 def $ text "Two"
      ui_ $ MenuItem 3 def $ text "Three"
      ui_ $ MenuItem 4 def $ text "Four"

    return dynVal
  |]


  ui_ $ Example "Secondary Menu" (def & subtitle ?~ text "A menu can adjust its appearance to de-emphasize its contents" & dynamic ?~ dynCode)
   [resetExample|
  \resetEvent -> do

    let menuConfig = def
          & secondary |~ True
          & value . event ?~ (Nothing <$ resetEvent)

    (dynVal, search) <- ui $ Menu menuConfig $ do
      ui_ $ MenuItem "home" def $ text "Home"
      ui_ $ MenuItem "messages" def $ text "Messages"
      ui_ $ MenuItem "friends" def $ text "Friends"
      ui $ Menu (def & right |~ True) $ do
        search <- ui $ MenuItem' def $ do
          ui $ Input (def & icon |?~ LeftIcon) $ do
            ui $ Icon "search" def
            textInput $ def & value . event ?~ ("" <$ resetEvent)
                            & placeholder |~ "Search..."
        ui_ $ MenuItem "logout" def $ text "Logout"
        return $ search ^. value

    return $ (,) <$> dynVal <*> search
  |]

  resetEvent <- ui $ Button def $ text "s"
  do
--  ui_ $ Example "Pointing Menu" (def & subtitle ?~ text "A menu can point to show its relationship to nearby content" & dynamic ?~ dynCode)
--   [resetExample|
--  \resetEvent -> do

    let menuConfig = mkMenuConfig (Identity ("home" :: Text))
          & pointing |~ True
          & value . event ?~ (Identity "home" <$ resetEvent)

    (dynVal, search) <- ui $ Menu menuConfig $ do
      ui_ $ MenuItem "home" def $ text "Home"
      ui_ $ MenuItem "messages" def $ text "Messages"
      ui_ $ MenuItem "friends" def $ text "Friends"
      ui $ Menu (mkMenuConfig (Identity "search") & right |~ True) $
        ui $ MenuItem' def $ do
          search <- ui $ Input (def & icon |?~ LeftIcon) $ do
            ui $ Icon "search" def
            textInput $ def & value . event ?~ ("" <$ resetEvent)
                            & placeholder |~ "Search..."

          return $ search ^. value

    ui $ Segment def $ paragraph $ do
      display' dynVal
      text " content..."

    return $ (,) <$> dynVal <*> search
--  |]

  (dynVal, result) <- ui $ Menu (mkMenuConfig Nothing) $ do

    ui $ Header (def & icon ?~ Icon "smile" def) $ text "hi"
    ui $ MenuItem 1 def $ text "One"
    ui $ MenuItem 2 def $ text "Two"
    ui $ Menu def $ do
      ui $ MenuItem' def $ ui $ Input def $ textInput def
      ui $ MenuItem 3 def $ text "Logout"

  display dynVal
  return ()

  ui_ $ Example "Vertical Menu" (def & subtitle ?~ text "A vertical menu displays elements vertically" & dynamic ?~ dynCode)
   [resetExample|
  \resetEvent -> do
    let counter txt = let widget = count <=< ui $ Button def $ text $ Static txt
                      in join <$> widgetHold widget (widget <$ resetEvent)

    inboxCount <- counter "Add inbox item"
    spamCount <- counter "Add spam item"
    updatesCount <- counter "Add updates item"

    let mkLabel dCount mColor = Label (labelConfig dCount mColor)
          $ text $ Dynamic $ tshow <$> dCount

        labelConfig dCount mColor = def
          & color |~ mColor
          & transition ?~ (def & initialDirection .~ Out
                               & event .~ fmap mkTransition (updated dCount))

        mkTransition count
          | count > 0 = Animation Jiggle $ def
              & cancelling .~ True & direction ?~ In & duration .~ 0.4
          | otherwise = Transition Fade $ def
              & cancelling .~ True & direction ?~ Out

        menuConfig = def & vertical .~ True
                   & value . event ?~ (Just "inbox" <$ resetEvent)
                   & value . initial ?~ "inbox"

    (selected, search) <- ui $ Menu menuConfig $ do
      ui $ MenuItem "inbox" (def & color ?~ Teal) $ do
        text "Inbox"
        ui $ mkLabel inboxCount $ Just Teal
      ui $ MenuItem "spam" def $ do
        text "Spam"
        ui $ mkLabel spamCount Nothing
      ui $ MenuItem "updates" def $ do
        text "Updates"
        ui $ mkLabel updatesCount Nothing
      ui $ MenuItem' def $
        ui $ Input (def & transparent |~ True & icon |?~ RightIcon) $ do
          ui $ Icon "search" def
          textInput $ def & placeholder |~ "Search mail..."

    return $ (,) <$> selected <*> view value search
 |]

-}
