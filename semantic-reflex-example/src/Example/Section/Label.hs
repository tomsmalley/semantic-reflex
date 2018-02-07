{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}

module Example.Section.Label where

import Control.Lens
import Data.Foldable (traverse_)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Reflex.Dom.SemanticUI
import Reflex.Dom.Core (text)

import Example.QQ
import Example.Common

labels :: forall t m. MonadWidget t m => Section t m
labels = Section "Label" blank $ do

  hscode $ $(printDefinition id stripParens ''LabelConfig)

  pageHeader H3 def $ text "Types"

  mkExample "Label" (def
    & subtitle ?~ text "A label")
    [example|
  label def $ do
    icon "mail" def
    text "23"
  |]

  mkExample "Image" (def
    & subtitle ?~ text "A label can emphasize an image")
    [example|
  label (def & labelImage |~ True) $ do
    image "images/animals/duck.png" def
    text "Donald"
  label (def & labelImage |~ True) $ do
    image "images/animals/sheep.png" def
    text "Sam"
  label (def & labelImage |~ True) $ do
    image "images/animals/bee.png" def
    text "Betty"
  |]

  mkExample "Pointing" (def
    & subtitle ?~ text "A label can point to adjacent content")
    [example|
  input (def & inputFluid |~ True) $ textInput def
  label (def & labelPointing |?~ AbovePointing) $
    text "Please enter a value"

  divider def

  label (def & labelPointing |?~ BelowPointing) $
    text "Please enter a value"
  input (def & inputFluid |~ True) $ textInput def

  divider def

  input def $ textInput def
  label (def & labelPointing |?~ LeftPointing) $
    text "That name is taken!"

  divider def

  label (def & labelPointing |?~ RightPointing) $
    text "Your password must be 6 characters or more"
  input def $ textInput def
  |]

  mkExample "Corner" (def
    & subtitle ?~ text "A label can position itself in the corner or an element")
    [example|
  let conf = def & imageShape |?~ Rounded
                 & style |~ Style "overflow: hidden"

  contentImage "images/animals/flamingo.png" conf $
    label (def & labelCorner |?~ LeftCorner
               & labelColor |?~ Pink
               & labelLink .~ True) $ icon "heart" def
  divider $ def & dividerHidden |~ True
  contentImage "images/animals/shark.png" conf $
    label (def & labelCorner |?~ RightCorner
               & labelColor |?~ Blue
               & labelLink .~ True) $ icon "heart" def
  |]

  mkExample "Tag" (def
    & subtitle ?~ text "A label can appear as a tag")
    [example|
  label (def & labelTag |~ True) $ text "New"
  label (def & labelColor |?~ Red & labelTag |~ True
             & labelLink .~ True) $ text "Upcoming"
  label (def & labelColor |?~ Teal & labelTag |~ True
             & labelLink .~ True) $ text "Featured"
  |]

  mkExample "Ribbon" (def
    & subtitle ?~ text "A label can appear as a ribbon")
    [example|
  divClass "ui two column padded grid" $ do
    divClass "column" $ do
      segment (def & segmentRaised |~ True) $ do
        label (def & labelLink .~ True & labelColor |?~ Red
                   & labelRibbon |?~ LeftRibbon) $ text "Overview"
        text "Account details"
        divider def
        label (def & labelLink .~ True & labelColor |?~ Blue
                   & labelRibbon |?~ LeftRibbon) $ text "Community"
        text "User Reviews"
    divClass "column" $ do
      segment def $ do
        label (def & labelLink .~ True & labelColor |?~ Orange
                   & labelRibbon |?~ RightRibbon) $ text "Specs"
        divider def
        label (def & labelLink .~ True & labelColor |?~ Teal
                   & labelRibbon |?~ RightRibbon) $ text "Reviews"
  |]

  mkExample "" def
    [example|
  contentImage "images/animals/duck.png" def $ do
    label (def & labelLink .~ True & labelColor |?~ Yellow
               & labelRibbon |?~ LeftRibbon) $ do
      icon "tag" def
      text "Duck"

  divider $ def & dividerHidden |~ True

  contentImage "images/animals/dinosaur.png" def $ do
    label (def & labelLink .~ True & labelColor |?~ Blue
               & labelRibbon |?~ LeftRibbon) $ do
      icon "tag" def
      text "Dinosaur"
  |]

  mkExample "Attached" (def
    & subtitle ?~ text "Labels can attach themselves to content segments")
    [example|
  divClass "ui two column grid" $ do
    divClass "column" $ segment def $ do
      label (def & labelAttached |?~ (def & vertically .~ TopAttached)) $ text "HTML"
      paragraph $ text "Hypertext Markup Language"
    divClass "column" $ segment def $ do
      label (def & labelAttached |?~ (def & vertically .~ BottomAttached)) $ text "CSS"
      paragraph $ text "Cascading Style Sheets"
  divClass "ui four column grid" $ do
    divClass "column" $ segment def $ do
        label (def & labelAttached |?~ (def & horizontally ?~ LeftAttached)) $ text "Top Left"
        paragraph $ text "Top Left Attached"
    divClass "column" $ segment def $ do
        label (def & labelAttached |?~ LabelAttached TopAttached (Just RightAttached)) $ text "Top Right"
        paragraph $ text "Top Right Attached"
    divClass "column" $ segment def $ do
        label (def & labelAttached |?~ LabelAttached BottomAttached (Just LeftAttached)) $ text "Bottom Left"
        paragraph $ text "Bottom Left Attached"
    divClass "column" $ segment def $ do
        label (def & labelAttached |?~ LabelAttached BottomAttached (Just RightAttached)) $ text "Bottom Right"
        paragraph $ text "Bottom Right Attached"
  |]

  mkExample "Horizontal" (def
    & subtitle ?~ text "A horizontal label is formatted to label content along-side it horizontally")
    [example|
  list (def & listDivided |~ True & listSelection |~ True) $ do
    let conf = def & labelHorizontal |~ True
    listItem def $ do
      label (conf & labelColor |?~ Red) $ text "Fruit"
      text "Strawberries"
    listItem def $ do
      label (conf & labelColor |?~ Purple) $ text "Sweets"
      text "Starburst"
    listItem def $ do
      label (conf & labelColor |?~ Red) $ text "Fruit"
      text "Raspberries"
    listItem def $ do
      label (conf & labelColor |?~ Blue) $ text "Animal"
      text "Dog"
  |]

  mkExample "Floating" (def
    & subtitle ?~ text "A label can float above another element")
    [example|
  segments (def & segmentsCompact |~ True) $ do
    segment def $ do
      icon "mail" def
      text "Messages"
      label (def & labelFloating |~ True & labelColor |?~ Red) $ text "22"
    segment def $ do
      icon "users" def
      text "Friends"
      label (def & labelFloating |~ True & labelColor |?~ Teal) $ text "14"
  |]

  mkExample "Detail" (def
    & subtitle ?~ text "A label can include a detail")
    [example|
  label (def & labelColor |?~ Orange & labelImage |~ True) $ do
    image "images/animals/mouse.png" def
    text "Michael"
    detail "Mouse"

  label (def & labelColor |?~ Yellow & labelImage |~ True) $ do
    image "images/animals/spider.png" def
    text "Sabrina"
    detail "Spider"

  label (def & labelColor |?~ Blue & labelImage |~ True) $ do
    image "images/animals/wolf.png" def
    text "William"
    detail "Wolf"
  |]

  mkExample "Icon" (def
    & subtitle ?~ text "A label can have an icon")
    ([str|
  \resetEvent -> do
    let src animal = "images/animals/" <> T.toLower animal <> ".png"
        mkLabel (name, animal, mColor) = mdo
          let
            conf = def
              & labelColor |~ mColor
              & labelImage |~ True
              & transition ?~ (def & transConfigEvent ?~ (leftmost
              [ Transition Instant (def & transitionDirection ?~ In) <$ resetEvent
              , Transition Scale (def & transitionDirection ?~ Out) <$ eClose
              ]))
          eClose <- label conf $ do
            image (pure $ src animal) def
            text name
            domEvent Click <$> icon' "delete" def
          pure ()

    traverse_ mkLabel
      [ ("Freddy", "Fox", Nothing)
      , ("Charlotte", "Camel", Just Teal)
      , ("Lenny", "Leopard", Just Blue)
      , ("Danielle", "Dog", Just Yellow)
      , ("Peter", "Parrot", Just Orange)
      , ("Cathy", "Cat", Just Red)
      ]
  |], Right $ \resetEvent -> do
    let src animal = "images/animals/" <> T.toLower animal <> ".png"
        mkLabel (name, animal, mColor) = mdo
          let
            conf = def
              & labelColor |~ mColor
              & labelImage |~ True
              & action ?~ (def & actionEvent ?~ (leftmost
              [ Transition Instant (def & transitionDirection ?~ In) <$ resetEvent
              , Transition Scale (def & transitionDirection ?~ Out) <$ eClose
              ]))
          eClose <- label conf $ do
            image (pure $ src animal) def
            text name
            domEvent Click <$> icon' "delete" def
          pure ()

    traverse_ mkLabel
      [ ("Freddy", "Fox", Nothing)
      , ("Charlotte", "Camel", Just Teal)
      , ("Lenny", "Leopard", Just Blue)
      , ("Danielle", "Dog", Just Yellow)
      , ("Peter", "Parrot", Just Orange)
      , ("Cathy", "Cat", Just Red)
      ]
    )

  mkExample "Image" (def
    & inbetween ?~ (message (def & messageType |?~ WarningMessage) $ do
        paragraph $ do
          icon "warning sign" def
          text "Images must have a 'Spaced' attribute when inside labels or they will cause a line break." )
    & subtitle ?~ text "A label can include an image")
    [example|
  label def $ do
    image "images/animals/kangaroo.png" $ def
      & imageShape |?~ Rounded & imageSpaced |?~ RightSpaced
    text "Kevin"
  label def $ do
    text "Billy"
    image "images/animals/bug.png" $ def & imageSpaced |?~ Spaced & imageShape |?~ Rounded
    text "Bug"
  label def $ do
    text "Dorothy"
    image "images/animals/dove.png" $ def
      & imageShape |?~ Rounded & imageSpaced |?~ LeftSpaced
  |]

  return ()

