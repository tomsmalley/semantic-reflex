{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}

module Example.Section.Transition where

import Control.Lens
import Control.Monad ((<=<), void, when, join)
import Data.Char (isUpper)
import Data.Foldable (traverse_)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.SemanticUI
import GHC.Tuple

import Example.QQ
import Example.Common

spaceBeforeUpper :: String -> String
spaceBeforeUpper "" = ""
spaceBeforeUpper (c:cs)
  | isUpper c = ' ' : c : spaceBeforeUpper cs
  | otherwise = c : spaceBeforeUpper cs

animalUrl :: Text -> Active t Text
animalUrl a = Static $ "images/animals/" <> a <> ".png"

mkButton :: MonadWidget t m => TransitionType -> Restrict None m (Event t Transition)
mkButton transType = do
  run <- ui $ Button (Static $ T.pack $ spaceBeforeUpper $ show transType) $ def
    & style |~ Style ("margin-bottom" =: "1em")
    & size |?~ Tiny
    & compact |~ True
  return $ Transition transType def <$ run

transitions :: forall t m. MonadWidget t m => Section m
transitions = LinkedSection "Transition" (simpleLink "https://semantic-ui.com/modules/transition.html") $ do

  ui $ Message $ def
    & messageType |?~ InfoMessage
    & message ?~ (do
      ui $ Icon "announcement" def
      text "The implementation of the Transition module does not depend on the Semantic UI or jQuery Javascript libraries.")

  ui $ Paragraph $ text "Any of the components in this library are capable of Semantic UI transitions. The difference between animations and transitions is more clear cut, with different data constructors for each:"

  hscode $(printDefinition id stripParens ''Transition)
  hscode $(printDefinition oneline stripParens ''TransitionType)
  hscode $(printDefinition id stripParens ''TransitionConfig)

  hscode $(printDefinition oneline stripParens ''AnimationType)
  hscode $(printDefinition id stripParens ''AnimationConfig)

  ui $ Paragraph $ text "If the direction of a transition event is not specified, the transition will flip the current state of the element. Animation events will cause hidden elements to be shown, and they will remain shown after the animation finishes."

  ui $ Paragraph $ text "Animation or Transition events are placed into a queue which allows time for each transition to finish. The animation or transition speeds are specified in seconds using the duration config fields."

  ui $ PageHeader H3 def $ ui $ Content def $ text "Examples"

  divClass "ui equal width grid" $ do

    divClass "row" $ divClass "column" $ do
      exampleCard "Animations" "An element can be animated to draw attention to it" [mkExample|
      let img (anim, animal) = divClass "center aligned column" $ do
            run <- ui $ Button (Static $ T.pack $ show anim) $ def
              & style |~ Style ("margin-bottom" =: "1em")
            ui $ Image (animalUrl animal) $ def
              & size |?~ Small
              & shape |?~ Rounded
              & transition . event ?~ (Animation anim def <$ run)

      divClass "ui six column vertically padded grid" $
        traverse_ img $ zip [Jiggle .. Bounce]
          ["duck", "cat", "eagle", "koala", "tiger", "kangaroo"]
      |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Transitions" "An element can be hidden or shown using a transition" [mkExample|
        toggleButton <- ui $ Button "Toggle" def
        showButton <- ui $ Button "Show" def
        hideButton <- ui $ Button "Hide" def
        ui $ Image (animalUrl "crocodile") $ def
          & size |?~ Small
          & shape |?~ Rounded
          & transition . event ?~ leftmost
            [ Transition Instant def <$ toggleButton
            , Transition Instant (def & direction ?~ In) <$ showButton
            , Transition Instant (def & direction ?~ Out) <$ hideButton
            ]
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Scale" "An element can scale in or out" [mkExample|
        evt <- mkButton Scale
        ui $ Image (animalUrl "gorilla") $ def
          & size |?~ Small
          & shape |?~ Rounded
          & transition . event ?~ evt
        |]

      divClass "column" $ do
        exampleCard "Fade" "An element can fade in or out" [mkExample|
        evts <- traverse mkButton [Fade, FadeLeft, FadeRight, FadeUp, FadeDown]
        ui $ Image (animalUrl "cow") $ def
          & size |?~ Small
          & shape |?~ Rounded
          & transition . event ?~ leftmost evts
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Flip" "An element can flip in or out" [mkExample|
        evts <- traverse mkButton [HorizontalFlip, VerticalFlip]
        ui $ Image (animalUrl "panda") $ def
          & size |?~ Small
          & shape |?~ Rounded
          & transition . event ?~ leftmost evts
        |]

      divClass "column" $ do
        exampleCard "Drop" "An element can drop in or out" [mkExample|
        evt <- mkButton Drop
        ui $ Image (animalUrl "turtle") $ def
          & size |?~ Small
          & shape |?~ Rounded
          & transition . event ?~ evt
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Fly" "An element can fly in or out" [mkExample|
        evts <- traverse mkButton [FlyLeft, FlyRight, FlyUp, FlyDown]
        ui $ Image (animalUrl "bird") $ def
          & size |?~ Small
          & shape |?~ Rounded
          & transition . event ?~ leftmost evts
        |]

      divClass "column" $ do
        exampleCard "Swing" "An element can swing in or out" [mkExample|
        evts <- traverse mkButton [SwingLeft, SwingRight, SwingUp, SwingDown]
        ui $ Image (animalUrl "monkey") $ def
          & size |?~ Small
          & shape |?~ Rounded
          & transition . event ?~ leftmost evts
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Browse" "An element can browse in or out" [mkExample|
        evts <- traverse mkButton [Browse, BrowseRight]
        ui $ Image (animalUrl "frog") $ def
          & size |?~ Small
          & shape |?~ Rounded
          & transition . event ?~ leftmost evts
        |]

      divClass "column" $ do
        exampleCard "Slide" "An element can slide in or out" [mkExample|
        evts <- traverse mkButton [SlideDown, SlideUp, SlideLeft, SlideRight]
        ui $ Image (animalUrl "wolf") $ def
          & size |?~ Small
          & shape |?~ Rounded
          & transition . event ?~ leftmost evts
        |]
