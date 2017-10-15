{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE PolyKinds      #-}

module Example.Section.Transition where

import Control.Lens
import Data.Char (isUpper)
import Data.Foldable (traverse_)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.SemanticUI

import Example.QQ
import Example.Common

spaceBeforeUpper :: String -> String
spaceBeforeUpper "" = ""
spaceBeforeUpper (c:cs)
  | isUpper c = ' ' : c : spaceBeforeUpper cs
  | otherwise = c : spaceBeforeUpper cs

animalUrl :: Text -> Active t Text
animalUrl a = Static $ "images/animals/" <> a <> ".png"

mkButtons :: MonadWidget t m => [TransitionType]
          -> Restrict None m (Event t Transition)
mkButtons [] = return never
mkButtons (transType:[]) = do
  evt <- reRestrict $ mkButton transType
  ui $ Divider $ def & hidden |~ True
  return evt
mkButtons types = do
  evt <- ui $ Buttons buttonsConfig $ leftmost <$> traverse mkButton types
  ui $ Divider $ def & hidden |~ True
  return evt
  where buttonsConfig = def & width |?~ toEnum (length types) & size |?~ Small

mkButton :: MonadWidget t m => TransitionType
         -> Restrict Buttons m (Event t Transition)
mkButton t = do
  run <- ui $ Button (def & fluid |~ True) $ do
    text $ Static $ T.pack $ spaceBeforeUpper $ show t
  return $ Transition t def <$ run

transitions :: forall t m. MonadWidget t m => Section m
transitions = LinkedSection "Transition" (simpleLink "https://semantic-ui.com/modules/transition.html") $ do

  ui $ Message (def & messageType |?~ InfoMessage) $ paragraph $ do
    ui $ Icon "announcement" def
    text "The implementation of the Transition module does not depend on the Semantic UI or jQuery Javascript libraries."

  ui $ Paragraph $ text "Any of the components in this library are capable of Semantic UI transitions. The difference between animations and transitions is more clear cut, with different data constructors for each:"

  hscode $(printDefinition id stripParens ''Transition)
  hscode $(printDefinition oneline stripParens ''TransitionType)
  hscode $(printDefinition id stripParens ''TransitionConfig)

  hscode $(printDefinition oneline stripParens ''AnimationType)
  hscode $(printDefinition id stripParens ''AnimationConfig)

  ui $ Paragraph $ text "If the direction of a transition event is not specified, the transition will flip the current state of the element. Animation events will cause hidden elements to be shown, and they will remain shown after the animation finishes."

  ui $ Paragraph $ text "Animation or Transition events are placed into a queue which allows time for each transition to finish. The animation or transition speeds are specified in seconds using the duration config fields."

  ui $ PageHeader H3 def $ text "Examples"

  ui $ Example "Animations" (def
    & subtitle ?~ text "An element can be animated to draw attention to it")
    [example|
  let img (anim, animal) = divClass "center aligned column" $ do
        run <- ui $ Button (def & style |~ Style ("margin-bottom" =: "1em")) $ do
          text $ Static $ T.pack $ show anim
        ui $ Image (animalUrl animal) $ def
          & size |?~ Small
          & shape |?~ Rounded
          & transition . event ?~ (Animation anim def <$ run)

  divClass "ui six column vertically padded grid" $
    traverse_ img $ zip [Jiggle .. Bounce]
      ["duck", "cat", "eagle", "koala", "tiger", "kangaroo"]
  |]

  ui $ Example "Transitions" (def
    & subtitle ?~ text "An element can be hidden or shown using a transition")
    [example|
  eTransition <- ui $ Buttons def $ do
    eToggle <- ui $ Button def $ text "Toggle"
    eShow <- ui $ Button def $ text "Show"
    eHide <- ui $ Button def $ text "Hide"
    return $ leftmost
      [ Transition Instant def <$ eToggle
      , Transition Instant (def & direction ?~ In) <$ eShow
      , Transition Instant (def & direction ?~ Out) <$ eHide
      ]

  ui $ Divider $ def & hidden |~ True

  ui $ Image (animalUrl "crocodile") $ def
    & size |?~ Small
    & shape |?~ Rounded
    & transition . event ?~ eTransition
  |]

  ui $ Example "Scale" (def
    & subtitle ?~ text "An element can scale in or out")
    [example|
  evt <- mkButtons [Scale]
  ui $ Image (animalUrl "gorilla") $ def
    & size |?~ Small
    & shape |?~ Rounded
    & transition . event ?~ evt
  |]

  ui $ Example "Fade" (def
    & subtitle ?~ text "An element can fade in or out")
    [example|
  evt <- mkButtons [Fade, FadeLeft, FadeRight, FadeUp, FadeDown]
  ui $ Image (animalUrl "cow") $ def
    & size |?~ Small
    & shape |?~ Rounded
    & transition . event ?~ evt
  |]

  ui $ Example "Flip" (def
    & subtitle ?~ text "An element can flip in or out")
    [example|
  evt <- mkButtons [HorizontalFlip, VerticalFlip]
  ui $ Image (animalUrl "panda") $ def
    & size |?~ Small
    & shape |?~ Rounded
    & transition . event ?~ evt
  |]

  ui $ Example "Drop" (def
    & subtitle ?~ text "An element can drop in or out")
    [example|
  evt <- mkButtons [Drop]
  ui $ Image (animalUrl "turtle") $ def
    & size |?~ Small
    & shape |?~ Rounded
    & transition . event ?~ evt
  |]

  ui $ Example "Fly" (def
    & subtitle ?~ text "An element can fly in or out")
    [example|
  evt <- mkButtons [FlyLeft, FlyRight, FlyUp, FlyDown]
  ui $ Image (animalUrl "bird") $ def
    & size |?~ Small
    & shape |?~ Rounded
    & transition . event ?~ evt
  |]

  ui $ Example "Swing" (def
    & subtitle ?~ text "An element can swing in or out")
    [example|
  evt <- mkButtons [SwingLeft, SwingRight, SwingUp, SwingDown]
  ui $ Image (animalUrl "monkey") $ def
    & size |?~ Small
    & shape |?~ Rounded
    & transition . event ?~ evt
  |]

  ui $ Example "Browse" (def
    & subtitle ?~ text "An element can browse in or out")
    [example|
  evt <- mkButtons [Browse, BrowseRight]
  ui $ Image (animalUrl "frog") $ def
    & size |?~ Small
    & shape |?~ Rounded
    & transition . event ?~ evt
  |]

  ui $ Example "Slide" (def
    & subtitle ?~ text "An element can slide in or out")
    [example|
  evt <- mkButtons [SlideDown, SlideUp, SlideLeft, SlideRight]
  ui $ Image (animalUrl "wolf") $ def
    & size |?~ Small
    & shape |?~ Rounded
    & transition . event ?~ evt
  |]
