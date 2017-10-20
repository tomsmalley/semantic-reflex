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
import Control.Monad ((<=<), void, join)
import Data.Text (Text)
import Reflex.Dom.SemanticUI
import GHC.Tuple

import Example.QQ
import Example.Common

data Favourite
  = Haskell
  | Semantic
  | Reflex
  deriving (Eq, Show)

menu :: forall t m. MonadWidget t m => Section t m
menu = LinkedSection "Menu" (text "A menu displays grouped navigation actions") $ do

  paragraph $ text "In Semantic UI menus are just exposed as styling elements and any active state must be managed by you. Here the state is managed for you."

  hscode $(printDefinition id stripParens ''Menu)
--  hscode $(printDefinition id stripParens ''MenuDef)
--  hscode $(printDefinition id id ''MenuItems)

  hscode $(printDefinition id stripParens ''MenuConfig)
  hscode $(printDefinition id stripParens ''MenuItemConfig)


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

  ui_ $ Example "Pointing Menu" (def & subtitle ?~ text "A menu can point to show its relationship to nearby content" & dynamic ?~ dynCode)
   [resetExample|
  \resetEvent -> do

    let menuConfig = def
          & pointing |~ True
          & value . event ?~ (Nothing <$ resetEvent)

    (dynVal, search) <- ui $ Menu menuConfig $ do
      ui_ $ MenuItem "home" def $ text "Home"
      ui_ $ MenuItem "messages" def $ text "Messages"
      ui_ $ MenuItem "friends" def $ text "Friends"
      ui $ Menu (def & right |~ True) $ do
        ui $ MenuItem' def $ do
          search <- ui $ Input (def & icon |?~ LeftIcon) $ do
            ui $ Icon "search" def
            textInput $ def & value . event ?~ ("" <$ resetEvent)
                            & placeholder |~ "Search..."

          return $ search ^. value

    ui $ Segment def $ do
      paragraph $ do
        display' dynVal
        text " content..."

    return $ (,) <$> dynVal <*> search
  |]

  (dynVal, result) <- ui $ Menu def $ do

    ui $ Header (def & icon ?~ Icon "smile" def) $ text "hi"
    ui $ MenuItem 1 def $ text "One"
    ui $ MenuItem 2 def $ text "Two"
    ui $ Menu def $ do
      ui $ MenuItem' def $ ui $ Input def $ textInput def
      ui $ MenuItem 3 def $ text "Logout"

  Component $ display dynVal
  return ()

  resetEvent <- ui $ Button def $ text "Reset"
  void $ do
--  exampleCardDyn dynCode "Vertical Menu" "A vertical menu displays elements vertically" [mkExample|
--  \resetEvent -> do
    let counter txt = let widget = count <=< ui $ Button def $ text $ Static txt
                      in join <$> widgetHold' widget (widget <$ resetEvent)

    inboxCount <- counter "Add inbox item"
    spamCount <- counter "Add spam item"
    updatesCount <- counter "Add updates item"

    let mkLabel dCount mColor = Label (labelConfig dCount mColor) (text $ Dynamic $ tshow <$> dCount)
        labelConfig dCount mColor = def
          & color |~ mColor
          & transition ?~ (def & event .~ (leftmost
            [ Animation Jiggle (def & duration .~ 0.4) <$ (ffilter id $ updated $ (> 0) <$> dCount)
            , Transition Fade (def & direction ?~ Out) <$ (ffilter id $ updated $ (<= 0) <$> dCount)
            ]))

        conf = def & vertical .~ True
                   & value . event ?~ (Just "inbox" <$ resetEvent)
                   & value . initial ?~ "inbox"

    (selected, _) <- ui $ Menu conf $ do
      ui $ MenuItem ("inbox" :: Text) (def & color ?~ Teal) $ do
        text "Inbox"
        ui $ mkLabel inboxCount $ Just Teal
      ui $ MenuItem ("spam" :: Text) def $ do
        text "Spam"
        ui $ mkLabel spamCount Nothing
      ui $ MenuItem ("updates" :: Text) def $ do
        text "Updates"
        ui $ mkLabel updatesCount Nothing

    return selected

