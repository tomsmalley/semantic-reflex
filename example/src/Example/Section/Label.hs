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
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Reflex.Dom.SemanticUI

import Example.QQ
import Example.Common

labels :: forall t m. MonadWidget t m => Section t m
labels = LinkedSection "Label" blank $ do

  hscode $ $(printDefinition id stripParens ''Label)
  hscode $ $(printDefinition id stripParens ''LabelConfig)

  ui_ $ PageHeader H3 def $ text "Types"

  ui_ $ Example "Label" (def
    & subtitle ?~ text "A label")
    [example|
  ui_ $ Label def $ do
    ui_ $ Icon "mail" def
    text "23"
  |]

  ui_ $ Example "Image" (def
    & subtitle ?~ text "A label can emphasize an image")
    [example|
  ui_ $ Label (def & image |~ True) $ do
    ui_ $ Image "images/animals/duck.png" def
    text "Donald"
  ui_ $ Label (def & image |~ True) $ do
    ui_ $ Image "images/animals/sheep.png" def
    text "Sam"
  ui_ $ Label (def & image |~ True) $ do
    ui_ $ Image "images/animals/bee.png" def
    text "Betty"
  |]

  ui_ $ Example "Pointing" (def
    & subtitle ?~ text "A label can point to adjacent content")
    [example|
  ui_ $ Input $ def & fluid |~ True
  ui_ $ Label (def & image |~ True & pointing |?~ AbovePointing) $ do
    text "Please enter a value"

  ui_ $ Divider def

  ui_ $ Label (def & image |~ True & pointing |?~ BelowPointing) $ do
    text "Please enter a value"
  ui_ $ Input $ def & fluid |~ True

  ui_ $ Divider def

  ui_ $ Input def
  ui_ $ Label (def & image |~ True & pointing |?~ LeftPointing) $ do
    text "That name is taken!"

  ui_ $ Divider def

  ui_ $ Label (def & image |~ True & pointing |?~ RightPointing) $ do
    text "Your password must be 6 characters or more"
  ui_ $ Input def
  |]

  ui_ $ Example "Corner" (def
    & subtitle ?~ text "A label can position itself in the corner or an element")
    [example|
  ui_ $ ContentImage "images/animals/flamingo.png" (def & spaced |?~ Spaced & shape |?~ Rounded & style |~ Style ("overflow" =: "hidden")) $ do
    ui_ $ Label (def & corner |?~ LeftCorner & color |?~ Pink & link .~ True) $ do
      ui_ $ Icon "heart" def

  ui_ $ ContentImage "images/animals/shark.png" (def & spaced |?~ Spaced & shape |?~ Rounded & style |~ Style ("overflow" =: "hidden")) $ do
    ui_ $ Label (def & corner |?~ RightCorner & color |?~ Blue & link .~ True) $ do
      ui_ $ Icon "heart" def
  |]

  ui_ $ Example "Tag" (def
    & subtitle ?~ text "A label can appear as a tag")
    [example|
  ui_ $ Label (def & tag |~ True) $ text "New"
  ui_ $ Label (def & color |?~ Red & tag |~ True & link .~ True) $ text "Upcoming"
  ui_ $ Label (def & color |?~ Teal & tag |~ True & link .~ True) $ text "Featured"
  |]

  ui_ $ Example "Ribbon" (def
    & subtitle ?~ text "A label can appear as a ribbon")
    [example|
  divClass "ui_ two column padded grid" $ do
    divClass "column" $ do
      ui_ $ Segment (def & raised |~ True) $ do
        ui_ $ Label (def & link .~ True & color |?~ Red & ribbon |?~ LeftRibbon) $ text "Overview"
        text "Account details"
        ui_ $ Divider def
        ui_ $ Label (def & link .~ True & color |?~ Blue & ribbon |?~ LeftRibbon) $ text "Community"
        text "User Reviews"
    divClass "column" $ do
      ui_ $ Segment def $ do
        ui_ $ Label (def & link .~ True & color |?~ Orange & ribbon |?~ RightRibbon) $ text "Specs"
        ui_ $ Divider def
        ui_ $ Label (def & link .~ True & color |?~ Teal & ribbon |?~ RightRibbon) $ text "Reviews"
  |]

  ui_ $ Example "" def
    [example|
  ui_ $ ContentImage "images/animals/duck.png" def $ do
    ui_ $ Label (def & link .~ True & color |?~ Yellow & ribbon |?~ LeftRibbon) $ do
      ui_ $ Icon "tag" def
      text "Duck"

  ui_ $ Divider $ def & hidden |~ True

  ui_ $ ContentImage "images/animals/dinosaur.png" def $ do
    ui_ $ Label (def & link .~ True & color |?~ Blue & ribbon |?~ LeftRibbon) $ do
      ui_ $ Icon "tag" def
      text "Dinosaur"
  |]

  ui_ $ Example "Attached" (def
    & subtitle ?~ text "Labels can attach themselves to content segments")
    [example|
  divClass "ui_ two column grid" $ do
    divClass "column" $ ui_ $ Segment def $ do
      ui_ $ Label (def & attached |?~ (def & vertically .~ TopAttached)) $ text "HTML"
      paragraph $ text "Hypertext Markup Language"
    divClass "column" $ ui_ $ Segment def $ do
      ui_ $ Label (def & attached |?~ (def & vertically .~ BottomAttached)) $ text "CSS"
      paragraph $ text "Cascading Style Sheets"
  divClass "ui_ four column grid" $ do
    divClass "column" $ do
      ui_ $ Segment def $ do
        ui_ $ Label (def & attached |?~ (def & horizontally ?~ LeftAttached)) $ text "Top Left"
        paragraph $ text "Top Left Attached"
    divClass "column" $ do
      ui_ $ Segment def $ do
        ui_ $ Label (def & attached |?~ LabelAttached TopAttached (Just RightAttached)) $ text "Top Right"
        paragraph $ text "Top Right Attached"
    divClass "column" $ do
      ui_ $ Segment def $ do
        ui_ $ Label (def & attached |?~ LabelAttached BottomAttached (Just LeftAttached)) $ text "Bottom Left"
        paragraph $ text "Bottom Left Attached"
    divClass "column" $ do
      ui_ $ Segment def $ do
        ui_ $ Label (def & attached |?~ LabelAttached BottomAttached (Just RightAttached)) $ text "Bottom Right"
        paragraph $ text "Bottom Right Attached"
  |]

  ui_ $ Example "Horizontal" (def
    & subtitle ?~ text "A horizontal label is formatted to label content along-side it horizontally")
    [example|
  divClass "ui_ divided selection list" $ do
    divClass "item" $ do
      ui_ $ Label (def & color |?~ Red & horizontal |~ True) $ text "Fruit"
      text "Strawberries"
    divClass "item" $ do
      ui_ $ Label (def & color |?~ Purple & horizontal |~ True) $ text "Sweets"
      text "Starburst"
    divClass "item" $ do
      ui_ $ Label (def & color |?~ Red & horizontal |~ True) $ text "Fruit"
      text "Raspberries"
    divClass "item" $ do
      ui_ $ Label (def & color |?~ Blue & horizontal |~ True) $ text "Animal"
      text "Dog"
  |]

  ui_ $ Example "Floating" (def
    & subtitle ?~ text "A label can float above another element")
    [example|
  ui_ $ Menu (def & compact .~ True) $ do
    ui_ $ MenuItem (1 :: Int) def $ do
      ui_ $ Icon "mail" def
      text "Messages"
      ui_ $ Label (def & floating |~ True & color |?~ Red) $ text "22"
    ui_ $ MenuItem 2 def $ do
      ui_ $ Icon "users" def
      text "Friends"
      ui_ $ Label (def & floating |~ True & color |?~ Teal) $ text "14"
  |]

  ui_ $ Example "Detail" (def
    & subtitle ?~ text "A label can include a detail")
    [example|
  ui_ $ Label (def & color |?~ Orange & image |~ True) $ do
    ui_ $ Image "images/animals/mouse.png" def
    text "Michael"
    ui_ $ Detail "Mouse"

  ui_ $ Label (def & color |?~ Yellow & image |~ True) $ do
    ui_ $ Image "images/animals/spider.png" def
    text "Sabrina"
    ui_ $ Detail "Spider"

  ui_ $ Label (def & color |?~ Blue & image |~ True) $ do
    ui_ $ Image "images/animals/wolf.png" def
    text "William"
    ui_ $ Detail "Wolf"
  |]

  ui_ $ Example "Icon" (def
    & subtitle ?~ text "A label can have an icon")
    ([str|
  \resetEvent -> do
    let src animal = "images/animals/" <> T.toLower animal <> ".png"
        mkLabel (name, animal, mColor) = do
          rec let conf = def
                    & color |~ mColor
                    & image |~ True
                    & transition ?~ (def & event .~ (leftmost
                    [ Transition Instant (def & direction ?~ In) <$ resetEvent
                    , Transition Scale (def & direction ?~ Out) <$ eClose
                    ]))

              eClose <- ui_ $ Label conf $ do
                ui_ $ Image (pure $ src animal) def
                text name
                (e, _) <- ui' $ Icon "delete" def
                return $ domEvent Click e

          return ()

    mapM_ mkLabel
      [ ("Freddy", "Fox", Nothing)
      , ("Charlotte", "Camel", Just Teal)
      , ("Lenny", "Leopard", Just Blue)
      , ("Danielle", "Dog", Just Yellow)
      , ("Peter", "Parrot", Just Orange)
      , ("Cathy", "Cat", Just Red)
      ]
  |], Right $ \resetEvent -> do
    let src animal = "images/animals/" <> T.toLower animal <> ".png"
        mkLabel (name, animal, mColor) = do
          rec let conf = def
                    & color |~ mColor
                    & image |~ True
                    & transition ?~ (def & event .~ (leftmost
                    [ Transition Instant (def & direction ?~ In) <$ resetEvent
                    , Transition Scale (def & direction ?~ Out) <$ eClose
                    ]))

              eClose <- ui $ Label conf $ do
                ui_ $ Image (pure $ src animal) def
                text name
                (e, _) <- ui' $ Icon "delete" def
                return $ domEvent Click e

          return ()

    mapM_ mkLabel
      [ ("Freddy", "Fox", Nothing)
      , ("Charlotte", "Camel", Just Teal)
      , ("Lenny", "Leopard", Just Blue)
      , ("Danielle", "Dog", Just Yellow)
      , ("Peter", "Parrot", Just Orange)
      , ("Cathy", "Cat", Just Red)
      ]
    )

  ui_ $ Example "Image" (def
    & subtitle ?~ text "A label can include an image")
    [example|
  ui_ $ Label def $ do
    ui_ $ Image "images/animals/kangaroo.png" (def & shape |?~ Rounded & spaced |?~ RightSpaced)
    text "Kevin"
  ui_ $ Label def $ do
    text "Billy"
    ui_ $ Image "images/animals/bug.png" (def & shape |?~ Rounded)
    text "Bug"
  ui_ $ Label def $ do
    text "Dorothy"
    ui_ $ Image "images/animals/dove.png" (def & shape |?~ Rounded & spaced |?~ LeftSpaced)
  |]

