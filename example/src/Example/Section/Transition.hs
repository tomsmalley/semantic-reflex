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

mkButton :: MonadWidget t m => TransitionType
         -> Component Buttons m (Event t Transition)
mkButton t = do
  run <- ui $ Button (def & fluid |~ True) $ do
    text $ Static $ T.pack $ spaceBeforeUpper $ show t
  return $ Transition t def <$ run

mkImages :: MonadWidget t m => Text -> Event t Transition -> Component None m ()
mkImages animal evt = do
  ui $ Divider $ def & hidden |~ True
  let imgConfig =  def
        & size |?~ Small & shape |?~ Rounded & inline |~ True
  ui $ Image (animalUrl animal) $ imgConfig
    & spaced |?~ RightSpaced
    & transition ?~ (def & event .~ evt)
  ui $ Image (animalUrl animal) $ imgConfig
    & spaced |?~ LeftSpaced
    & transition ?~ (def & event .~ fmap (\(Transition t c) -> Transition t $ c & cancelling .~ True) evt)

mkButtons :: MonadWidget t m => Text -> [TransitionType] -> Component None m ()
mkButtons _ [] = return ()
mkButtons animal (transType:[]) = do
  evt <- reComponent $ mkButton transType
  mkImages animal evt
mkButtons animal types = do
  evt <- ui $ Buttons buttonsConfig $ leftmost <$> traverse mkButton types
  mkImages animal evt
  where buttonsConfig = def & width |?~ toEnum (length types) & size |?~ Small

transitions :: forall t m. MonadWidget t m => Section t m
transitions = LinkedSection "Transition" (simpleLink "https://semantic-ui.com/modules/transition.html") $ do

  ui $ Message (def & messageType |?~ InfoMessage) $ paragraph $ do
    ui $ Icon "announcement" def
    text "The implementation of the Transition module does not depend on the Semantic UI or jQuery Javascript libraries."

  paragraph $ text "Any of the components in this library are capable of Semantic UI transitions. The difference between animations and transitions is more clear cut, with different data constructors for each:"

  hscode $(printDefinition oneline stripParens ''TransitionType)
  hscode $(printDefinition oneline stripParens ''AnimationType)

  hscode $(printDefinition id stripParens ''Transition)
  hscode $(printDefinition id stripParens ''TransitionConfig)

  paragraph $ text "If the direction of a transition event is not specified, the transition will flip the current state of the element. Animation events will cause hidden elements to be shown, and they will remain shown after the animation finishes."

  paragraph $ text "Animation or Transition events are placed into a queue which allows time for each transition to finish. The animation or transition speeds are specified in seconds using the duration config fields."

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
          & transition ?~ (def & event .~ (Animation anim def <$ run))

  divClass "ui six column vertically padded grid" $
    traverse_ img $ zip [Jiggle .. Bounce]
      ["duck", "cat", "eagle", "koala", "tiger", "kangaroo"]
  |]

  ui $ Example "Cancelling Animations" (def
    & subtitle ?~ text "Transitions can cancel override queued transitions")
    [example|
  let img (anim, animal) = divClass "center aligned column" $ do
        run <- ui $ Button (def & style |~ Style ("margin-bottom" =: "1em")) $ do
          text $ Static $ T.pack $ show anim
        ui $ Image (animalUrl animal) $ def
          & size |?~ Small
          & shape |?~ Rounded
          & transition ?~ (def & event .~ (Animation anim (def & cancelling .~ True) <$ run))

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
    & transition ?~ (def & event .~ eTransition)
  |]

  ui $ Example "Scale" (def
    & subtitle ?~ text "An element can scale in or out")
    [example|
  mkButtons "gorilla" [Scale]
  |]

  ui $ Example "Fade" (def
    & subtitle ?~ text "An element can fade in or out")
    [example|
  mkButtons "cow" [Fade, FadeLeft, FadeRight, FadeUp, FadeDown]
  |]

  ui $ Example "Flip" (def
    & subtitle ?~ text "An element can flip in or out")
    [example|
  mkButtons "panda" [HorizontalFlip, VerticalFlip]
  |]

  ui $ Example "Drop" (def
    & subtitle ?~ text "An element can drop in or out")
    [example|
  mkButtons "turtle" [Drop]
  |]

  ui $ Example "Fly" (def
    & subtitle ?~ text "An element can fly in or out")
    [example|
  mkButtons "bird" [FlyLeft, FlyRight, FlyUp, FlyDown]
  |]

  ui $ Example "Swing" (def
    & subtitle ?~ text "An element can swing in or out")
    [example|
  mkButtons "monkey" [SwingLeft, SwingRight, SwingUp, SwingDown]
  |]

  ui $ Example "Browse" (def
    & subtitle ?~ text "An element can browse in or out")
    [example|
  mkButtons "frog" [Browse, BrowseRight]
  |]

  ui $ Example "Slide" (def
    & subtitle ?~ text "An element can slide in or out")
    [example|
  mkButtons "wolf" [SlideDown, SlideUp, SlideLeft, SlideRight]
  |]
