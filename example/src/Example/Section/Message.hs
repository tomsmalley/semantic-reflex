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

import GHC.Tuple -- TH requires this for (,)
import Control.Lens
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Reflex.Dom.SemanticUI

import Example.QQ
import Example.Common

messages :: forall t m. MonadWidget t m => Section m
messages = LinkedSection "Message" (simpleLink "https://semantic-ui.com/collections/message.html") $ do

  hscode $(printDefinition id stripParens ''Message)
  hscode $(printDefinition id stripParens ''MessageConfig)
  hscode $(printDefinition oneline id ''MessageType)
  hscode $(printDefinition id stripParens ''MessageResult)

  ui $ Header H3 (text "Examples") def

  exampleCard "Message" "A basic message" $ [mkExample|
  ui $ Message $ def
    & header ?~ text "Changes in Service"
    & message ?~ text "We just updated our privacy policy here to better service our customers. We recommend reviewing the changes."
  |]

  ui $ Message $ def
    & message ?~ ( do
        ui_ $ Icon "warning sign" def
        text "The list message implementation is not complete." )
    & messageType |?~ WarningMessage

  exampleCard "List Message" "A message with a list" $ [mkExample|
  ui $ Message $ def
    & header ?~ text "New Site Features"
    & message ?~ ( elClass "ul" "list" $ do
      el "li" $ text "You can now have cover images on blog pages"
      el "li" $ text "Drafts will now auto-save while writing" )
  |]

  exampleCard "Icon Message" "A message can contain an icon" $ [mkExample|
  ui $ Message $ def
    & icon ?~ Icon "inbox" def
    & header ?~ text "Have you heard about our mailing list?"
    & message ?~ text "Get the best news in your e-mail every day."
  ui $ Message $ def
    & icon ?~ Icon "notched circle loading" def
    & header ?~ text "Just one second"
    & message ?~ text "We're fetching that content for you."
  |]

  exampleCard "Hidable Block" "A message can be hidden" $ [mkExample|
  hideEvent <- ui $ Button "Hide" $ def
    & attached |?~ Horizontally LeftAttached
  showEvent <- ui $ Button "Show" $ def
    & attached |?~ Horizontally RightAttached
  ui $ Message $ def
    & setHidden .~ leftmost [True <$ hideEvent, False <$ showEvent]
    & message ?~ text "This message is controlled by the buttons above."
  |]

  exampleCardReset "Dismissable Block" "A message that the user can choose to hide" $ [mkExample|
  \resetEvent -> do
    ui $ Message $ def
      & dismissable .~ True
      & setHidden .~ (False <$ resetEvent)
      & header ?~ text "Welcome back!"
      & message ?~ text "This is a special notification which you can dismiss if you're bored with it."
  |]

  exampleCard "Floating" "A message can float above content that it is related to" $ [mkExample|
  ui $ Message $ def
    & floating |~ True
    & message ?~ text "Way to go!"
  |]

  exampleCard "Compact" "A message can only take up the width of its content" $ [mkExample|
  ui $ Message $ def
    & compact |~ True
    & message ?~ text "Get all the best inventions in your e-mail every day. Sign up now!"
  |]

  exampleCard "Attached" "A message can be formatted to attach itself to other content" $ [mkExample|
  ui $ Message $ def
    & attached |?~ TopAttached
    & header ?~ text "Welcome to our site!"
    & message ?~ text "Fill out the form below to sign-up for a new account"
  divClass "ui attached segment" $ text "Content"
  ui $ Message $ def
    & attached |?~ BottomAttached
    & messageType |?~ WarningMessage
    & message ?~ ( do
      ui $ Icon "help" def
      text "Already signed up? "
      el "a" $ text "Login here"
      text " instead." )
  |]

  exampleCardReset "Warning" "A message may be formatted to display warning messages" $ [mkExample|
  \resetEvent -> do
    ui $ Message $ def
      & dismissable .~ True
      & setHidden .~ (False <$ resetEvent)
      & messageType |?~ WarningMessage
      & header ?~ text "You must register before you can do that!"
      & message ?~ text "Visit our registration page, then try again"
  |]

  exampleCardReset "Info" "A message may be formatted to display information" $ [mkExample|
  \resetEvent -> do
    ui $ Message $ def
      & dismissable .~ True
      & setHidden .~ (False <$ resetEvent)
      & messageType |?~ InfoMessage
      & header ?~ text "Was this what you wanted?"
      & message ?~ text "It's good to see you again."
  |]

  exampleCardReset "Positive / Success" "A message may be formatted to display a positive message" $ [mkExample|
  \resetEvent -> do
    ui $ Message $ def
      & dismissable .~ True
      & setHidden .~ (False <$ resetEvent)
      & messageType |?~ PositiveMessage
      & header ?~ text "You are eligible for a reward"
      & message ?~ (text "Go to your " >> el "b" (text "special offers") >> text " page to see now.")
    ui $ Message $ def
      & dismissable .~ True
      & setHidden .~ (False <$ resetEvent)
      & messageType |?~ SuccessMessage
      & header ?~ text "Your user registration was successful."
      & message ?~ text "You may now log-in with the username you have chosen"
  |]

  exampleCardReset "Negative / Error" "A message may be formatted to display a negative message" $ [mkExample|
  \resetEvent -> do
    ui $ Message $ def
      & dismissable .~ True
      & setHidden .~ (False <$ resetEvent)
      & messageType |?~ NegativeMessage
      & header ?~ text "I'm sorry Dave, I'm afraid I can't do that."
      & message ?~ text "I think you know what the problem is just as well as I do."
    ui $ Message $ def
      & dismissable .~ True
      & setHidden .~ (False <$ resetEvent)
      & messageType |?~ ErrorMessage
      & header ?~ text "Therer were some errors with your submission"
      & message ?~ text "You need to select your home country."
  |]

  exampleCard "Colored" "A message can be formatted to be different colors" $ [mkExample|
  let putMessage c = ui $ Message $ def
        & color |?~ c
        & message ?~ text (tshow c)
  traverse putMessage [Red .. Black]
  |]

  exampleCard "Size" "A message can have different sizes" $ [mkExample|
  let putMessage s = ui $ Message $ def
        & size |?~ s
        & message ?~ text ("This is a " <> T.toLower (tshow s) <> " message.")
  traverse putMessage [Mini .. Massive]
  |]

  return ()

