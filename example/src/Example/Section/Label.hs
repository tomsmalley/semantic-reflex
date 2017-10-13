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
import Control.Monad ((<=<), void, when, join)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.SemanticUI

import Example.QQ
import Example.Common

labels :: forall t m. MonadWidget t m => Section m
labels = LinkedSection "Label" blank $ do

  hscode $ $(printDefinition id stripParens ''Label)
  hscode $ $(printDefinition id stripParens ''LabelConfig)

  ui $ PageHeader H3 def $ ui $ Content def $ text "Types"

  divClass "ui equal width stackable grid" $ do

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Label" "A label" [mkExample|
        ui $ Label def $ do
          ui $ Icon "mail" def
          text "23"
        |]

      divClass "column" $ do
        exampleCard "Image" "A label can emphasize an image" [mkExample|
        ui $ Label (def & image |~ True) $ do
          ui $ Image "images/animals/duck.png" def
          text "Donald"
        ui $ Label (def & image |~ True) $ do
          ui $ Image "images/animals/sheep.png" def
          text "Sam"
        ui $ Label (def & image |~ True) $ do
          ui $ Image "images/animals/bee.png" def
          text "Betty"
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Pointing" "A label can point to adjacent content" [mkExample|
        ui $ Input $ def & fluid |~ True
        ui $ Label (def & image |~ True & pointing |?~ AbovePointing) $ do
          text "Please enter a value"

        ui $ Divider def

        ui $ Label (def & image |~ True & pointing |?~ BelowPointing) $ do
          text "Please enter a value"
        ui $ Input $ def & fluid |~ True

        ui $ Divider def

        ui $ Input def
        ui $ Label (def & image |~ True & pointing |?~ LeftPointing) $ do
          text "That name is taken!"

        ui $ Divider def

        ui $ Label (def & image |~ True & pointing |?~ RightPointing) $ do
          text "Your password must be 6 characters or more"
        ui $ Input def
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Corner" "A label can position itself in the corner of an element" [mkExample|
        ui $ ContentImage "images/animals/flamingo.png" (def & spaced |?~ Spaced & shape |?~ Rounded & style |~ Style ("overflow" =: "hidden")) $ do
          ui $ Label (def & corner |?~ LeftCorner & color |?~ Pink & link .~ True) $ do
            ui $ Icon "heart" def

        ui $ ContentImage "images/animals/shark.png" (def & spaced |?~ Spaced & shape |?~ Rounded & style |~ Style ("overflow" =: "hidden")) $ do
          ui $ Label (def & corner |?~ RightCorner & color |?~ Blue & link .~ True) $ do
            ui $ Icon "heart" def
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Tag" "A label can appear as a tag" [mkExample|
        ui $ Label (def & tag |~ True) $ text "New"
        ui $ Label (def & color |?~ Red & tag |~ True & link .~ True) $ text "Upcoming"
        ui $ Label (def & color |?~ Teal & tag |~ True & link .~ True) $ text "Featured"
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Tag" "A label can appear as a tag" [mkExample|
        divClass "ui two column padded grid" $ do
          divClass "column" $ do
            ui $ Segment (def & raised |~ True) $ do
              ui $ Label (def & link .~ True & color |?~ Red & ribbon |?~ LeftRibbon) $ text "Overview"
              text "Account details"
              ui $ Divider def
              ui $ Label (def & link .~ True & color |?~ Blue & ribbon |?~ LeftRibbon) $ text "Community"
              text "User Reviews"
          divClass "column" $ do
            ui $ Segment def $ do
              ui $ Label (def & link .~ True & color |?~ Orange & ribbon |?~ RightRibbon) $ text "Specs"
              ui $ Divider def
              ui $ Label (def & link .~ True & color |?~ Teal & ribbon |?~ RightRibbon) $ text "Reviews"
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "" "" [mkExample|
        divClass "ui two column padded grid" $ do
          divClass "column" $ do
            ui $ ContentImage "images/animals/duck.png" def $ do
              ui $ Label (def & link .~ True & color |?~ Yellow & ribbon |?~ LeftRibbon) $ do
                ui $ Icon "tag" def
                text "Duck"
          divClass "column" $ do
            ui $ ContentImage "images/animals/dinosaur.png" def $ do
              ui $ Label (def & link .~ True & color |?~ Blue & ribbon |?~ LeftRibbon) $ do
                ui $ Icon "tag" def
                text "Dinosaur"
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Attached" "Labels can attach themselves to content segments" [mkExample|
        divClass "ui two column grid" $ do
          divClass "column" $ ui $ Segment def $ do
            ui $ Label (def & attached |?~ (def & vertically .~ TopAttached)) $ text "HTML"
            ui $ Paragraph $ text "Hypertext Markup Language"
          divClass "column" $ ui $ Segment def $ do
            ui $ Label (def & attached |?~ (def & vertically .~ BottomAttached)) $ text "CSS"
            ui $ Paragraph $ text "Cascading Style Sheets"
        divClass "ui four column grid" $ do
          divClass "column" $ do
            ui $ Segment def $ do
              ui $ Label (def & attached |?~ (def & horizontally ?~ LeftAttached)) $ text "Top Left"
              ui $ Paragraph $ text "Top Left Attached"
          divClass "column" $ do
            ui $ Segment def $ do
              ui $ Label (def & attached |?~ LabelAttached TopAttached (Just RightAttached)) $ text "Top Right"
              ui $ Paragraph $ text "Top Right Attached"
          divClass "column" $ do
            ui $ Segment def $ do
              ui $ Label (def & attached |?~ LabelAttached BottomAttached (Just LeftAttached)) $ text "Bottom Left"
              ui $ Paragraph $ text "Bottom Left Attached"
          divClass "column" $ do
            ui $ Segment def $ do
              ui $ Label (def & attached |?~ LabelAttached BottomAttached (Just RightAttached)) $ text "Bottom Right"
              ui $ Paragraph $ text "Bottom Right Attached"
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Horizontal" "A horizontal label is formatted to label content along-side it horizontally" [mkExample|
        divClass "ui divided selection list" $ do
          divClass "item" $ do
            ui $ Label (def & color |?~ Red & horizontal |~ True) $ text "Fruit"
            text "Strawberries"
          divClass "item" $ do
            ui $ Label (def & color |?~ Purple & horizontal |~ True) $ text "Sweets"
            text "Starburst"
          divClass "item" $ do
            ui $ Label (def & color |?~ Red & horizontal |~ True) $ text "Fruit"
            text "Raspberries"
          divClass "item" $ do
            ui $ Label (def & color |?~ Blue & horizontal |~ True) $ text "Animal"
            text "Dog"
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Floating" "A label can float above another element" [mkExample|
        ui $ Menu (def & compact .~ True) $ do
          ui $ MenuItem 1 def $ do
            ui $ Icon "mail" def
            text "Messages"
            ui $ Label (def & floating |~ True & color |?~ Red) $ text "22"
          ui $ MenuItem 2 def $ do
            ui $ Icon "users" def
            text "Friends"
            ui $ Label (def & floating |~ True & color |?~ Teal) $ text "14"
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Detail" "A label can include a detail" [mkExample|
        ui $ Label (def & color |?~ Orange & image |~ True) $ do
          ui $ Image "images/animals/mouse.png" def
          text "Michael"
          ui $ Detail "Mouse"

        ui $ Label (def & color |?~ Yellow & image |~ True) $ do
          ui $ Image "images/animals/spider.png" def
          text "Sabrina"
          ui $ Detail "Spider"

        ui $ Label (def & color |?~ Blue & image |~ True) $ do
          ui $ Image "images/animals/wolf.png" def
          text "William"
          ui $ Detail "Wolf"
        |]

    divClass "row" $ do
      divClass "column" $ do
        -- This example is manual due to 'rec' not being supported by TH
        exampleCardReset "Icon" "A label can have an icon" ([str|
        \resetEvent -> do
          let src animal = "images/animals/" <> T.toLower animal <> ".png"
              mkLabel (name, animal, mColor) = do
                rec let conf = def
                          & color |~ mColor
                          & image |~ True
                          & transition . event ?~ leftmost
                          [ Transition Instant (def & direction ?~ In) <$ resetEvent
                          , Transition Scale (def & direction ?~ Out) <$ eClose
                          ]

                    eClose <- ui $ Label conf $ do
                      ui $ Image (pure $ src animal) def
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
        |], \resetEvent -> do
          let src animal = "images/animals/" <> T.toLower animal <> ".png"
              mkLabel (name, animal, mColor) = do
                rec let conf = def
                          & color |~ mColor
                          & image |~ True
                          & transition . event ?~ leftmost
                          [ Transition Instant (def & direction ?~ In) <$ resetEvent
                          , Transition Scale (def & direction ?~ Out) <$ eClose
                          ]

                    eClose <- ui $ Label conf $ do
                      ui $ Image (pure $ src animal) def
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

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Image" "A label can include an image" [mkExample|
        ui $ Label def $ do
          ui $ Image "images/animals/kangaroo.png" (def & shape |?~ Rounded & spaced |?~ RightSpaced)
          text "Kevin"
        ui $ Label def $ do
          text "Billy"
          ui $ Image "images/animals/bug.png" (def & shape |?~ Rounded)
          text "Bug"
        ui $ Label def $ do
          text "Dorothy"
          ui $ Image "images/animals/dove.png" (def & shape |?~ Rounded & spaced |?~ LeftSpaced)
        |]

  return ()

