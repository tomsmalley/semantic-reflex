{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}

module Example.Section.Transition where

import Control.Lens
import Control.Monad ((<=<), void, when, join)
import Data.Foldable (traverse_)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.SemanticUI

import Example.QQ
import Example.Common

transitions :: forall t m. MonadWidget t m => Section m
transitions = LinkedSection "Transition" (simpleLink "https://semantic-ui.com/modules/transition.html") $ do

  divClass "ui info message" $ do
    ui $ Icon "announcement" def
    text "The implementation of the Transition module does not depend on the Semantic UI or jQuery Javascript libraries."

  el "p" $ text "Any element is capable of animation with Transition or Animation. Animation or Transition events are placed into a queue which allows time for each transition to finish."

  hscode $ $(printDefinition oneline stripParens ''TransitionType)
  hscode $ $(printDefinition id stripParens ''TransitionConfig)

  hscode $ $(printDefinition oneline stripParens ''AnimationType)
  hscode $ $(printDefinition id stripParens ''AnimationConfig)
  hscode $ $(printDefinition id stripParens ''Animation)

  fire <- ui $ Button "Fire" def

  ui $ Header H3 (text "Examples") $ def & animation ?~ (Animation Bounce def <$ fire)

  exampleCard "Animation" "" [mkExample|
  let url a = "/images/animals/" <> a <> ".png"
      img (anim, animal) = divClass "center aligned column" $ do
        run <- ui $ Button (Static $ T.pack $ show anim) $ def
          & style |~ Style ("margin-bottom" =: "1em")
        ui $ Image (url animal) $ def
          & size |?~ Small
          & shape |~ Rounded
          & animation ?~ (Animation anim (def & duration .~ 1.5) <$ run)

  divClass "ui six column vertically padded grid" $
    traverse_ img $ zip [Jiggle .. Bounce] ["duck", "cat", "eagle", "koala", "tiger", "kangaroo"]
  |]

  let url a = "/images/animals/" <> a <> ".png"
      img (anim, animal) = divClass "center aligned column" $ do
        run <- ui $ Button (Static $ T.pack $ show anim) $ def
          & style |~ Style ("margin-bottom" =: "1em")
        flyOut <- ui $ Button "Fly Out" $ def
          & style |~ Style ("margin-bottom" =: "1em")
        flyIn <- ui $ Button "Fly In" $ def
          & style |~ Style ("margin-bottom" =: "1em")
        ui $ Image (url animal) $ def
          & size |?~ Small
          & shape |~ Rounded
          & transition ?~ leftmost
            [ Transition anim (def & duration .~ 1.5) <$ run
            , Transition FlyUp (def & direction ?~ Out) <$ flyOut
            , Transition FlyUp (def & direction ?~ In) <$ flyIn
            ]

  img (Fade, "fox")
