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
import Reflex.Dom.Core (text)

import Example.QQ
import Example.Common

messages :: forall t m. MonadWidget t m => Section t m
messages = Section "Message" (simpleLink "https://semantic-ui.com/collections/message.html") $ do

  hscode $(printDefinition id stripParens ''MessageConfig)
  hscode $(printDefinition oneline id ''MessageType)

  pageHeader H3 def $ text "Examples"

  mkExample "Message" (def
    & subtitle ?~ text "A basic message")
    [example|
  message def $ do
    header def $ text "Changes in Service"
    paragraph $ text "We just updated our privacy policy here to better service our customers. We recommend reviewing the changes."
  |]

  mkExample "List Message" (def
    & inbetween ?~ (message (def & messageConfig_type |?~ WarningMessage) $ do
        paragraph $ do
          icon "warning sign" def
          text "The list message implementation is not complete." )
    & subtitle ?~ text "A message with a list")
    [example|
  message def $ do
    header def $ text "New Site Features"
    list def $ do
      listItem def $ text "You can now have cover images on blog pages"
      listItem def $ text "Drafts will now auto-save while writing"
  |]

  mkExample "Icon Message" (def
    & subtitle ?~ text "A message can contain an icon")
    [example|
  message (def & messageConfig_icon ?~ Icon "inbox" def) $ do
    header def $ text "Have you heard about our mailing list?"
    paragraph $ text "Get the best news in your e-mail every day."
  message (def & messageConfig_icon ?~ Icon "notched circle loading" def) $ do
    header def $ text "Just one second"
    paragraph $ text "We're fetching that content for you."
  |]

  mkExample "Dismissable Message" (def
    & subtitle ?~ text "A message that the user can choose to hide")
    [resetExample|
  \resetEvent -> do
    let trans = Transition Fade $ def & transitionConfig_duration .~ 0.2
        config = def & action ?~ (def & action_event ?~ (Transition Instant
              (def & transitionConfig_direction ?~ In) <$ resetEvent))
    dismissableMessage trans config $ do
      header def $ text "Welcome back!"
      paragraph $ text "This is a special notification which you can dismiss if you're bored with it. It uses the given transition to hide the message when the user clicks on the close icon."
  |]

  mkExample "Floating" (def
    & subtitle ?~ text "A message can float above the page")
    [example|
  message (def & messageConfig_floating |~ True) $ do
    paragraph $ text "Way to go!"
  |]

  mkExample "Compact" (def
    & subtitle ?~ text "A message can only take up the width of its content")
    [example|
  message (def & messageConfig_compact |~ True) $ do
    paragraph $ text "Get all the best inventions in your e-mail every day. Sign up now!"
  |]

  mkExample "Attached" (def
    & subtitle ?~ text "A message can be formatted to attach itself to other content")
    [example|
  message (def & messageConfig_attached |?~ TopAttached) $ do
    header def $ text "Welcome to our site!"
    paragraph $ text "Fill out the form below to sign-up for a new account"

  segment (def & segmentConfig_attached |?~ Attached) $ text "Content"

  message (def & messageConfig_attached |?~ BottomAttached
               & messageConfig_type |?~ WarningMessage) $ do
    paragraph $ do
      icon "help" def
      text "Already signed up? "
      hyperlink "#" $ text "Login here"
      text " instead."
  |]

  mkExample "Warning" (def
    & subtitle ?~ text "A message may be formatted to display warning messages")
    [example|
  message (def & messageConfig_type |?~ WarningMessage) $ do
    header def $ text "You must register before you can do that!"
    paragraph $ text "Visit our registration page, then try again"
  |]

  mkExample "Info" (def
    & subtitle ?~ text "A message may be formatted to display information")
    [example|
  message (def & messageConfig_type |?~ InfoMessage) $ do
    header def $ text "Was this what you wanted?"
    paragraph $ text "It's good to see you again."
  |]

  mkExample "Positive / Success" (def
    & subtitle ?~ text "A message may be formatted to display a positive message")
    [example|
  message (def & messageConfig_type |?~ MessageType Positive) $ do
    header def $ text "You are eligible for a reward"
    paragraph $ do
      text "Go to your "
      el "b" $ text "special offers"
      text " page to see now."

  message (def & messageConfig_type |?~ MessageType Success) $ do
    header def $ text "Your user registration was successful."
    paragraph $ text "You may now log-in with the username you have chosen"
  |]

  mkExample "Negative / Error" (def
    & subtitle ?~ text "A message may be formatted to display a negative message")
    [example|
  message (def & messageConfig_type |?~ MessageType Negative) $ do
    header def $ text "I'm sorry Dave, I'm afraid I can't do that."
    paragraph $ text "I think you know what the problem is just as well as I do."

  message (def & messageConfig_type |?~ MessageType Error) $ do
    header def $ text "There were some errors with your submission"
    paragraph $ text "You need to select your home country."
  |]

  mkExample "Colored" (def
    & subtitle ?~ text "A message can be formatted to be different colors")
    [example|
  for_ [minBound .. maxBound] $ \c -> message (def & messageConfig_color |?~ c) $ do
    paragraph $ text $ tshow c
  |]

  mkExample "Size" (def
    & subtitle ?~ text "A message can have different sizes")
    [example|
  for_ [minBound .. maxBound] $ \s -> message (def & messageConfig_size |?~ s) $ do
    paragraph $ text $ "This is a " <> T.toLower (tshow s) <> " message."
  |]

  return ()

