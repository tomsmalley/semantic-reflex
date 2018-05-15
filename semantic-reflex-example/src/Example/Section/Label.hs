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
  label (def & labelConfig_image |~ True) $ do
    image def $ Left $ Img "images/animals/duck.png" def
    text "Donald"
  label (def & labelConfig_image |~ True) $ do
    image def $ Left $ Img "images/animals/sheep.png" def
    text "Sam"
  label (def & labelConfig_image |~ True) $ do
    image def $ Left $ Img "images/animals/bee.png" def
    text "Betty"
  |]

  mkExample "Pointing" (def
    & subtitle ?~ text "A label can point to adjacent content")
    [example|
  input (def & inputConfig_fluid |~ True) $ textInput def
  label (def & labelConfig_pointing |?~ AbovePointing) $
    text "Please enter a value"

  divider def

  label (def & labelConfig_pointing |?~ BelowPointing) $
    text "Please enter a value"
  input (def & inputConfig_fluid |~ True) $ textInput def

  divider def

  input def $ textInput def
  label (def & labelConfig_pointing |?~ LeftPointing) $
    text "That name is taken!"

  divider def

  label (def & labelConfig_pointing |?~ RightPointing) $
    text "Your password must be 6 characters or more"
  input def $ textInput def
  |]

  mkExample "Corner" (def
    & subtitle ?~ text "A label can position itself in the corner or an element")
    [example|
  let conf = def & imageConfig_shape |?~ Rounded
                 & style |~ Style "overflow: hidden"

  image conf $ Right $ do
    label (def & labelConfig_corner |?~ LeftCorner
               & labelConfig_color |?~ Pink
               & labelConfig_link .~ True) $ icon "heart" def
    img "images/animals/flamingo.png" def
  divider $ def & dividerConfig_hidden |~ True
  image conf $ Right $ do
    label (def & labelConfig_corner |?~ RightCorner
               & labelConfig_color |?~ Blue
               & labelConfig_link .~ True) $ icon "heart" def
    img "images/animals/shark.png" def
  |]

  mkExample "Tag" (def
    & subtitle ?~ text "A label can appear as a tag")
    [example|
  label (def & labelConfig_tag |~ True) $ text "New"
  label (def & labelConfig_color |?~ Red & labelConfig_tag |~ True
             & labelConfig_link .~ True) $ text "Upcoming"
  label (def & labelConfig_color |?~ Teal & labelConfig_tag |~ True
             & labelConfig_link .~ True) $ text "Featured"
  |]

  mkExample "Ribbon" (def
    & subtitle ?~ text "A label can appear as a ribbon")
    [example|
  divClass "ui two column padded grid" $ do
    divClass "column" $ do
      segment (def & segmentConfig_raised |~ True) $ do
        label (def & labelConfig_link .~ True & labelConfig_color |?~ Red
                   & labelConfig_ribbon |?~ LeftRibbon) $ text "Overview"
        text "Account details"
        divider def
        label (def & labelConfig_link .~ True & labelConfig_color |?~ Blue
                   & labelConfig_ribbon |?~ LeftRibbon) $ text "Community"
        text "User Reviews"
    divClass "column" $ do
      segment def $ do
        label (def & labelConfig_link .~ True & labelConfig_color |?~ Orange
                   & labelConfig_ribbon |?~ RightRibbon) $ text "Specs"
        divider def
        label (def & labelConfig_link .~ True & labelConfig_color |?~ Teal
                   & labelConfig_ribbon |?~ RightRibbon) $ text "Reviews"
  |]

  mkExample "" def
    [example|
  image def $ Right $ do
    label (def & labelConfig_link .~ True & labelConfig_color |?~ Yellow
               & labelConfig_ribbon |?~ LeftRibbon) $ do
      icon "tag" def
      text "Duck"
    img "images/animals/duck.png" def

  divider $ def & dividerConfig_hidden |~ True

  image def $ Right $ do
    label (def & labelConfig_link .~ True & labelConfig_color |?~ Blue
               & labelConfig_ribbon |?~ LeftRibbon) $ do
      icon "tag" def
      text "Dinosaur"
    img "images/animals/dinosaur.png" def
  |]

  mkExample "Attached" (def
    & subtitle ?~ text "Labels can attach themselves to content segments")
    [example|
  divClass "ui two column grid" $ do
    divClass "column" $ segment def $ do
      label (def & labelConfig_attached |?~ (def & labelAttached_vertically .~ TopAttached)) $ text "HTML"
      paragraph $ text "Hypertext Markup Language"
    divClass "column" $ segment def $ do
      label (def & labelConfig_attached |?~ (def & labelAttached_vertically .~ BottomAttached)) $ text "CSS"
      paragraph $ text "Cascading Style Sheets"
  divClass "ui four column grid" $ do
    divClass "column" $ segment def $ do
        label (def & labelConfig_attached |?~ (def & labelAttached_horizontally ?~ LeftAttached)) $ text "Top Left"
        paragraph $ text "Top Left Attached"
    divClass "column" $ segment def $ do
        label (def & labelConfig_attached |?~ LabelAttached TopAttached (Just RightAttached)) $ text "Top Right"
        paragraph $ text "Top Right Attached"
    divClass "column" $ segment def $ do
        label (def & labelConfig_attached |?~ LabelAttached BottomAttached (Just LeftAttached)) $ text "Bottom Left"
        paragraph $ text "Bottom Left Attached"
    divClass "column" $ segment def $ do
        label (def & labelConfig_attached |?~ LabelAttached BottomAttached (Just RightAttached)) $ text "Bottom Right"
        paragraph $ text "Bottom Right Attached"
  |]

  mkExample "Horizontal" (def
    & subtitle ?~ text "A horizontal label is formatted to label content along-side it horizontally")
    [example|
  list (def & listConfig_divided |~ True & listConfig_selection |~ True) $ do
    let conf = def & labelConfig_horizontal |~ True
    listItem def $ do
      label (conf & labelConfig_color |?~ Red) $ text "Fruit"
      text "Strawberries"
    listItem def $ do
      label (conf & labelConfig_color |?~ Purple) $ text "Sweets"
      text "Starburst"
    listItem def $ do
      label (conf & labelConfig_color |?~ Red) $ text "Fruit"
      text "Raspberries"
    listItem def $ do
      label (conf & labelConfig_color |?~ Blue) $ text "Animal"
      text "Dog"
  |]

  mkExample "Floating" (def
    & subtitle ?~ text "A label can float above another element")
    [example|
  segments (def & segmentsConfig_compact |~ True) $ do
    segment def $ do
      icon "mail" def
      text "Messages"
      label (def & labelConfig_floating |~ True & labelConfig_color |?~ Red) $ text "22"
    segment def $ do
      icon "users" def
      text "Friends"
      label (def & labelConfig_floating |~ True & labelConfig_color |?~ Teal) $ text "14"
  |]

  mkExample "Detail" (def
    & subtitle ?~ text "A label can include a detail")
    [example|
  label (def & labelConfig_color |?~ Orange & labelConfig_image |~ True) $ do
    image def $ Left $ Img "images/animals/mouse.png" def
    text "Michael"
    detail "Mouse"

  label (def & labelConfig_color |?~ Yellow & labelConfig_image |~ True) $ do
    image def $ Left $ Img "images/animals/spider.png" def
    text "Sabrina"
    detail "Spider"

  label (def & labelConfig_color |?~ Blue & labelConfig_image |~ True) $ do
    image def $ Left $ Img "images/animals/wolf.png" def
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
              & labelConfig_color |~ mColor
              & labelConfig_image |~ True
              & transition ?~ (def & transConfigEvent ?~ (leftmost
              [ Transition Instant (def & transitionConfig_direction ?~ In) <$ resetEvent
              , Transition Scale (def & transitionConfig_direction ?~ Out) <$ eClose
              ]))
          eClose <- label conf $ do
            image def $ Left $ Img (pure $ src animal) def
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
              & labelConfig_color |~ mColor
              & labelConfig_image |~ True
              & action ?~ (def & action_event ?~ (leftmost
              [ Transition Instant (def & transitionConfig_direction ?~ In) <$ resetEvent
              , Transition Scale (def & transitionConfig_direction ?~ Out) <$ eClose
              ]))
          eClose <- label conf $ do
            image def $ Left $ Img (pure $ src animal) def
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

  return ()

