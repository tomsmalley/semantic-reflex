{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}

{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Example.Section.Message where

import Control.Lens
import Data.Foldable (for_)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Reflex.Dom.SemanticUI

import Example.QQ
import Example.Common

messages :: forall t m. MonadWidget t m => Section t m
messages = LinkedSection "Message" (simpleLink "https://semantic-ui.com/collections/message.html") $ do

  hscode $(printDefinition id stripParens ''Message)
  hscode $(printDefinition id stripParens ''MessageConfig)
  hscode $(printDefinition oneline id ''MessageType)

  ui $ PageHeader H3 def $ text "Examples"

  ui $ Example "Message" (def
    & subtitle ?~ text "A basic message")
    [example|
  ui $ Message def $ do
    ui $ Header def $ text "Changes in Service"
    paragraph $ text "We just updated our privacy policy here to better service our customers. We recommend reviewing the changes."
  |]

  ui $ Example "List Message" (def
    & inbetween ?~ ( ui_ $ Message (def & messageType |?~ WarningMessage) $ do
        paragraph $ do
          ui_ $ Icon "warning sign" def
          text "The list message implementation is not complete." )
    & subtitle ?~ text "A message with a list")
    [example|
  ui $ Message def $ do
    ui $ Header def $ text "New Site Features"
    unUI $ elClass "ul" "list" $ do
      el "li" $ text "You can now have cover images on blog pages"
      el "li" $ text "Drafts will now auto-save while writing"
  |]

  ui $ Example "Icon Message" (def
    & subtitle ?~ text "A message can contain an icon")
    [example|
  ui $ Message (def & icon ?~ Icon "inbox" def) $ do
    ui $ Header def $ text "Have you heard about our mailing list?"
    paragraph $ text "Get the best news in your e-mail every day."
  ui $ Message (def & icon ?~ Icon "notched circle loading" def) $ do
    ui $ Header def $ text "Just one second"
    paragraph $ text "We're fetching that content for you."
  |]

  ui $ Example "Dismissable Message" (def
    & subtitle ?~ text "A message that the user can choose to hide")
    [resetExample|
  \resetEvent -> do
    let config = def
          & dismissable ?~ Transition Fade (def & duration .~ 0.2)
          & transition ?~ (def & event .~ (Transition Instant (def & direction ?~ In) <$ resetEvent))
    ui $ Message config $ do
      ui $ Header def $ text "Welcome back!"
      paragraph $ text "This is a special notification which you can dismiss if you're bored with it. The dismissable setting uses the given transition to hide the message when the user clicks on the close icon."
  |]

  ui $ Example "Floating" (def
    & subtitle ?~ text "A message can float above the page")
    [example|
  ui $ Message (def & floating |~ True) $ do
    paragraph $ text "Way to go!"
  |]

  ui $ Example "Compact" (def
    & subtitle ?~ text "A message can only take up the width of its content")
    [example|
  ui $ Message (def & compact |~ True) $ do
    paragraph $ text "Get all the best inventions in your e-mail every day. Sign up now!"
  |]

  ui $ Example "Attached" (def
    & subtitle ?~ text "A message can be formatted to attach itself to other content")
    [example|
  ui $ Message (def & attached |?~ TopAttached) $ do
    ui $ Header def $ text "Welcome to our site!"
    paragraph $ text "Fill out the form below to sign-up for a new account"

  ui $ Segment (def & attached |?~ Attached) $ text "Content"

  ui $ Message (def & attached |?~ BottomAttached & messageType |?~ WarningMessage) $ do
    paragraph $ do
      ui $ Icon "help" def
      text "Already signed up? "
      ui $ Anchor (text "Login here") def
      text " instead."
  |]

  ui $ Example "Warning" (def
    & subtitle ?~ text "A message may be formatted to display warning messages")
    [example|
  ui $ Message (def & messageType |?~ WarningMessage) $ do
    ui $ Header def $ text "You must register before you can do that!"
    paragraph $ text "Visit our registration page, then try again"
  |]

  ui $ Example "Info" (def
    & subtitle ?~ text "A message may be formatted to display information")
    [example|
  ui $ Message (def & messageType |?~ InfoMessage) $ do
    ui $ Header def $ text "Was this what you wanted?"
    paragraph $ text "It's good to see you again."
  |]

  ui $ Example "Positive / Success" (def
    & subtitle ?~ text "A message may be formatted to display a positive message")
    [example|
  ui $ Message (def & messageType |?~ MessageType Positive) $ do
    ui $ Header def $ text "You are eligible for a reward"
    paragraph $ do
      text "Go to your "
      el "b" $ text "special offers"
      text " page to see now."

  ui $ Message (def & messageType |?~ MessageType Success) $ do
    ui $ Header def $ text "Your user registration was successful."
    paragraph $ text "You may now log-in with the username you have chosen"
  |]

  ui $ Example "Negative / Error" (def
    & subtitle ?~ text "A message may be formatted to display a negative message")
    [example|
  ui $ Message (def & messageType |?~ MessageType Negative) $ do
    ui $ Header def $ text "I'm sorry Dave, I'm afraid I can't do that."
    paragraph $ text "I think you know what the problem is just as well as I do."

  ui $ Message (def & messageType |?~ MessageType Error) $ do
    ui $ Header def $ text "There were some errors with your submission"
    paragraph $ text "You need to select your home country."
  |]

  ui $ Example "Colored" (def
    & subtitle ?~ text "A message can be formatted to be different colors")
    [example|
  for_ [minBound .. maxBound] $ \c -> ui_ $ Message (def & color |?~ c) $ do
    paragraph $ text $ Static $ tshow c
  |]

  ui_ $ Example "Size" (def
    & subtitle ?~ text "A message can have different sizes")
    [example|
  for_ [minBound .. maxBound] $ \s -> ui_ $ Message (def & size |?~ s) $ do
    paragraph $ text $ Static $ "This is a " <> T.toLower (tshow s) <> " message."
  |]

