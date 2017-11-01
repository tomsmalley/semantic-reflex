{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}

module Example.Section.List where

import Control.Lens hiding (List)
import Data.Foldable (for_, traverse_)
import Data.Monoid ((<>))
import Reflex.Dom.SemanticUI

import Example.QQ
import Example.Common

data Files a = File a | Folder a [Files a]

lists :: MonadWidget t m => Section t m
lists = LinkedSection "List" (text "A list is used to group related content" >> simpleLink "https://semantic-ui.com/elements/list.html") $ do

  hscode $(printDefinition id stripParens ''List)
  hscode $(printDefinition id stripParens ''ListConfig)
  hscode $(printDefinition id stripParens ''ListItem)
  hscode $(printDefinition id stripParens ''ListItemConfig)

  ui $ Example "List" (def
    & subtitle ?~ text "A list groups related content")
    [example|
  ui $ List def $ for_ ["Apples", "Pears", "Oranges"] $ ui . ListItem def . text
  |]

  ui $ Example "" def
    [example|
  ui_ $ List def $ do
    ui_ $ ListItem (def & icon ?~ Icon "users" def) $ text "Semantic UI"
    ui_ $ ListItem (def & icon ?~ Icon "marker" def) $ text "New York, NY"
    ui_ $ ListItem (def & icon ?~ Icon "mail" def) $ ui_ $ Anchor (text "jack@semantic-ui.com") def
    ui_ $ ListItem (def & icon ?~ Icon "linkify" def) $ ui_ $ Anchor (text "semantic-ui.com") def
  |]

  ui $ Example "" def
    [example|
  ui_ $ List (def & divided |~ True & relaxed |?~ Relaxed) $ do

    let repos = [ ("Semantic-Org/Semantic-UI", 10 :: Int)
                , ("Semantic-Org/Semantic-UI-Docs", 22)
                , ("Semantic-Org/Semantic-UI-Meteor", 34) ]

    let github = Icon "github" $ def & size |?~ Large & classes |~ "middle aligned"

    for_ repos $ \(name, time) -> ui $ ListItem (def & icon ?~ github) $ do
      ui $ ListHeader $ text name
      ui $ ListDescription $ text $ Static $ "Updated " <> tshow time <> " mins ago"
  |]

  ui $ Example "" def
    [example|
  let
    -- data Files a = File a | Folder a [Files a]
    files =
      [ Folder ("src", "Source files for project")
        [ Folder ("site", "Your site's theme") []
        , Folder ("themes", "Packaged theme files")
          [ Folder ("default", "Default packaged theme") []
          , Folder ("my_theme", "Packaged themes are also available in this folder") [] ]
        , File ("theme.config", "Config file for setting packaged themes") ]
      , Folder ("dist", "Compiled CSS and JS files")
        [ Folder ("components", "Individual component CSS and JS") [] ]
      , File ("semantic.json", "Contains build settings for gulp") ]

    listFiles [] = Nothing
    listFiles fs = Just $ List def $ for_ fs $ \f -> case f of
      File (name, description) -> ui $ ListItem (def & icon ?~ Icon "file" def) $ do
        ui $ ListHeader $ text name
        ui $ ListDescription $ text description
      Folder (name, description) fs' -> ui $ ListItem (def & icon ?~ Icon "folder" def) $ do
        ui $ ListHeader $ text name
        ui $ ListDescription $ text description
        traverse_ ui $ listFiles fs'

  traverse_ ui $ listFiles files
  |]

  ui $ Example "Bulleted" (def
    & subtitle ?~ text "A list can mark items with a bullet")
    [example|
  ui_ $ List (def & listType |?~ Bulleted) $ do
    ui $ ListItem def $ text "Gaining Access"
    ui $ ListItem def $ text "Inviting Friends"
    ui $ ListItem def $ do
      text "Benefits"
      ui_ $ List def $ do
        ui $ ListItem def $ ui_ $ Anchor (text "Link to somewhere") def
        ui $ ListItem def $ text "Rebates"
        ui $ ListItem def $ text "Discounts"
    ui $ ListItem def $ text "Warranty"
  |]

  ui $ Example "" def
    [example|
  ui_ $ List (def & listType |?~ Bulleted & horizontal |~ True) $ do
    ui $ ListItem def $ ui_ $ Anchor (text "About Us") def
    ui $ ListItem def $ ui_ $ Anchor (text "Sitemap") def
    ui $ ListItem def $ ui_ $ Anchor (text "Contact") def
  |]

  ui $ Example "Ordered" (def
    & subtitle ?~ text "A list can be ordered numerically")
    [example|
  ui_ $ List (def & listType |?~ Ordered) $ do
    ui $ ListItem def $ ui_ $ Anchor (text "Getting Started") def
    ui $ ListItem def $ ui_ $ Anchor (text "Introduction") def
    ui $ ListItem def $ do
      ui_ $ Anchor (text "Languages") def
      ui $ List def $ do
        ui $ ListItem def $ ui_ $ Anchor (text "HTML") def
        ui $ ListItem def $ ui_ $ Anchor (text "Javascript") def
        ui $ ListItem def $ ui_ $ Anchor (text "CSS") def
    ui $ ListItem def $ ui_ $ Anchor (text "Review") def
  |]

  ui $ Example "Value" (def
    & subtitle ?~ text "A list item can have a custom value"
    & inbetween ?~ upstreamIssue 5911 "The data-value attribute is currently ignored for <div> list items.")
    [example|
  ui_ $ List (def & listType |?~ Ordered) $ do
    ui $ ListItem (def & attributes |~ "data-value" =: "*") $ text "Signing Up"
    ui $ ListItem (def & attributes |~ "data-value" =: "*") $ text "User Benefits"
    ui $ ListItem (def & attributes |~ "data-value" =: "*") $ do
      text "User Types"
      ui $ List def $ do
        ui $ ListItem (def & attributes |~ "data-value" =: "-") $ text "Admin"
        ui $ ListItem (def & attributes |~ "data-value" =: "-") $ text "Power User"
        ui $ ListItem (def & attributes |~ "data-value" =: "-") $ text "Regular User"
    ui $ ListItem (def & attributes |~ "data-value" =: "*") $ text "Deleting Your Account"
  |]

  ui $ Example "Link" (def
    & subtitle ?~ text "A list can be specially formatted for navigation links")
    [example|
  ui_ $ List (def & link |~ True) $ do
    ui $ ListItem (def & as .~ ListItemLink & classes |~ "active") $ text "Home"
    ui $ ListItem (def & as .~ ListItemLink) $ text "About"
    ui $ ListItem (def & as .~ ListItemLink) $ text "Jobs"
    ui $ ListItem (def & as .~ ListItemLink) $ text "Team"
  |]

  ui $ Example "Icon" (def
    & subtitle ?~ text "A list item can contain an icon")
    [example|
  ui_ $ List def $ do
    ui $ ListItem (def & as .~ ListItemLink & icon ?~ Icon "help" def) $ do
      ui $ ListHeader $ text "Floated Icon"
      ui $ ListDescription $ text "This text will always have a left margin to make sure it sits alongside your icon"

    ui $ ListItem (def & as .~ ListItemLink & icon ?~ Icon "right triangle" def) $ do
      ui $ ListHeader $ text "Icon Alignment"
      ui $ ListDescription $ text "Floated icons are by default top aligned"

    ui $ ListItem (def & icon ?~ Icon "help" def) $ text "Inline Text"
  |]

  ui $ Example "Image" (def
    & subtitle ?~ text "A list item can contain an image")
    [example|
  ui_ $ List def $ do

    let animalImage animal = Image ("images/animals/" <> animal <> ".png") $ def
          & shape |?~ Avatar

    ui $ ListItem (def & image ?~ animalImage "fox") $ do
      ui $ ListHeader $ ui_ $ Anchor (text "Fiona") def
      ui $ ListDescription $ text "Last seen rummaging through the bins just now."

    ui $ ListItem (def & image ?~ animalImage "elephant") $ do
      ui $ ListHeader $ ui_ $ Anchor (text "Ethan") def
      ui $ ListDescription $ text "Last seen playing in the water 2 hours ago."

    ui $ ListItem (def & image ?~ animalImage "monkey") $ do
      ui $ ListHeader $ ui_ $ Anchor (text "Marcus") def
      ui $ ListDescription $ text "Last seen swinging on his tyre yesterday."

    ui $ ListItem (def & image ?~ animalImage "giraffe") $ do
      ui $ ListHeader $ ui_ $ Anchor (text "Ginny") def
      ui $ ListDescription $ text "Last seen nibbling on a tree 3 days ago."

    ui $ ListItem (def & image ?~ animalImage "spider") $ do
      ui $ ListHeader $ ui_ $ Anchor (text "Stanley") def
      ui $ ListDescription $ text "Last seen spooking people a week ago."
  |]

  ui $ Example "Link" (def
    & subtitle ?~ text "A list can contain links")
    [example|
  ui_ $ List def $ do
    ui $ ListItem (def & as .~ ListItemLink) $ text "What is a FAQ?"
    ui $ ListItem (def & as .~ ListItemLink) $ text "Who is our user?"
    ui $ ListItem (def & as .~ ListItemLink) $ text "Where is our office located?"
  |]

  ui $ Example "Header" (def
    & subtitle ?~ text "A list item can contain a header")
    [example|
  ui_ $ List def $ do

    ui $ ListItem def $ do
      ui $ ListHeader $ text "Manchester"
      text "A lovely city"

    ui $ ListItem def $ do
      ui $ ListHeader $ text "Sheffield"
      text "Also quite a lovely city"

    ui $ ListItem def $ do
      ui $ ListHeader $ text "London"
      text "Sometimes can be a lovely city"

    ui $ ListItem def $ do
      ui $ ListHeader $ text "Bristol"
      text "What a lovely city"
  |]

  ui $ Example "Description" (def
    & subtitle ?~ text "A list item can contain a description")
    [example|
  ui_ $ List def $ do

    ui $ ListItem (def & icon ?~ Icon "map marker" def) $ do
      ui $ ListHeader $ ui_ $ Anchor (text "Krolewskie Jadlo") def
      ui $ ListDescription $ text "An excellent polish restaurant, quick delivery and hearty, filling meals."

    ui $ ListItem (def & icon ?~ Icon "map marker" def) $ do
      ui $ ListHeader $ ui_ $ Anchor (text "Sapporo Haru") def
      ui $ ListDescription $ text "Greenpoint's best choice for quick and delicious sushi."
  |]

  ui $ Example "Horizontal" (def
    & subtitle ?~ text "A list can be formatted to have items appear horizontally")
    [example|
  ui_ $ List (def & horizontal |~ True) $ do

    let animalImage animal = Image ("images/animals/" <> animal <> ".png") $ def
          & shape |?~ Avatar

    ui $ ListItem (def & image ?~ animalImage "eagle") $ do
      ui $ ListHeader $ text "Eddie"
      text "Top Contributor"

    ui $ ListItem (def & image ?~ animalImage "tiger") $ do
      ui $ ListHeader $ text "Tommy"
      text "Admin"

    ui $ ListItem (def & image ?~ animalImage "kangaroo") $ do
      ui $ ListHeader $ text "Katie"
      text "Top Rated User"
  |]

  ui $ Example "Inverted" (def
    & subtitle ?~ text "A list can be inverted to appear on a dark background")
    [example|
  ui_ $ Segment (def & inverted |~ True) $
    ui_ $ List (def & inverted |~ True & relaxed |?~ Relaxed & divided |~ True) $ do

      ui $ ListItem def $ do
        ui $ ListHeader $ text "Snickerdoodle"
        text "An excellent companion"

      ui $ ListItem def $ do
        ui $ ListHeader $ text "Poodle"
        text "A poodle, it's pretty basic"

      ui $ ListItem def $ do
        ui $ ListHeader $ text "Paulo"
        text "He's also a dog"
  |]

  ui $ Example "Selection" (def
    & subtitle ?~ text "A selection list formats list items as possible choices")
    [example|
  ui_ $ List (def & selection |~ True & aligned |?~ ListMiddle) $ do

    let animalImage animal = Image ("images/animals/" <> animal <> ".png") $ def
          & shape |?~ Avatar

    ui $ ListItem (def & image ?~ animalImage "horse") $
      ui $ ListHeader $ text "Helen"

    ui $ ListItem (def & image ?~ animalImage "chicken") $
      ui $ ListHeader $ text "Christian"

    ui $ ListItem (def & image ?~ animalImage "duck") $
      ui $ ListHeader $ text "Daniel"
  |]

  ui $ Example "Animated" (def
    & subtitle ?~ text "A list can animate to set the current item apart from the list")
    [example|
  ui_ $ List (def & animated |~ True & aligned |?~ ListMiddle) $ do

    let animalImage animal = Image ("images/animals/" <> animal <> ".png") $ def
          & shape |?~ Avatar

    ui $ ListItem (def & image ?~ animalImage "horse") $
      ui $ ListHeader $ text "Helen"

    ui $ ListItem (def & image ?~ animalImage "chicken") $
      ui $ ListHeader $ text "Christian"

    ui $ ListItem (def & image ?~ animalImage "duck") $
      ui $ ListHeader $ text "Daniel"
  |]

  ui $ Example "Relaxed" (def
    & subtitle ?~ text "A list can relax its padding to provide more negative space")
    [example|
  ui_ $ List def $ do
    ui $ ListItem def $ text "One"
    ui $ ListItem def $ text "Two"
    ui $ ListItem def $ text "Three"

  ui $ Divider def

  ui_ $ List (def & relaxed |?~ Relaxed) $ do
    ui $ ListItem def $ text "One"
    ui $ ListItem def $ text "Two"
    ui $ ListItem def $ text "Three"

  ui $ Divider def

  ui_ $ List (def & relaxed |?~ VeryRelaxed) $ do
    ui $ ListItem def $ text "One"
    ui $ ListItem def $ text "Two"
    ui $ ListItem def $ text "Three"

  ui $ Divider def

  ui_ $ List (def & horizontal |~ True) $ do
    ui $ ListItem def $ text "One"
    ui $ ListItem def $ text "Two"
    ui $ ListItem def $ text "Three"

  ui $ Divider def

  ui_ $ List (def & relaxed |?~ Relaxed & horizontal |~ True) $ do
    ui $ ListItem def $ text "One"
    ui $ ListItem def $ text "Two"
    ui $ ListItem def $ text "Three"

  ui $ Divider def

  ui_ $ List (def & relaxed |?~ VeryRelaxed & horizontal |~ True) $ do
    ui $ ListItem def $ text "One"
    ui $ ListItem def $ text "Two"
    ui $ ListItem def $ text "Three"
  |]

  ui $ Example "Divided" (def
    & subtitle ?~ text "A list can show divisions between items")
    [example|
  ui_ $ List (def & divided |~ True & aligned |?~ ListMiddle) $ do

    let animalImage animal = Image ("images/animals/" <> animal <> ".png") $ def
          & shape |?~ Avatar

    ui $ ListItem (def & image ?~ animalImage "cow") $
      ui $ ListHeader $ text "Charlie"

    ui $ ListItem (def & image ?~ animalImage "turtle") $
      ui $ ListHeader $ text "Tammy"

    ui $ ListItem (def & image ?~ animalImage "bear") $
      ui $ ListHeader $ text "Betty"

  ui $ Divider $ def & hidden |~ True

  ui_ $ List (def & divided |~ True & horizontal |~ True) $ do
    ui $ ListItem def $ text "About Us"
    ui $ ListItem def $ text "Contact"
    ui $ ListItem def $ text "Support"
  |]

  ui $ Example "Celled" (def
    & subtitle ?~ text "A list can divide its items into cells")
    [example|
  ui_ $ List (def & celled |~ True & aligned |?~ ListMiddle) $ do

    let animalImage animal = Image ("images/animals/" <> animal <> ".png") $ def
          & shape |?~ Avatar

    ui $ ListItem (def & image ?~ animalImage "cow") $ do
      ui $ ListHeader $ text "Charlie"
      text "A cow"

    ui $ ListItem (def & image ?~ animalImage "turtle") $ do
      ui $ ListHeader $ text "Tammy"
      text "A turtle"

    ui $ ListItem (def & image ?~ animalImage "bear") $ do
      ui $ ListHeader $ text "Betty"
      text "A bear"

  ui $ Divider $ def & hidden |~ True

  ui_ $ List (def & celled |~ True & horizontal |~ True) $ do
    ui $ ListItem def $ text "About Us"
    ui $ ListItem def $ text "Contact"
    ui $ ListItem def $ text "Support"
  |]

  ui $ Example "Size" (def
    & subtitle ?~ text "A list can vary in size")
    [example|
  let conf = def & horizontal |~ True & aligned |?~ ListMiddle

      animalImage animal = Image ("images/animals/" <> animal <> ".png") $ def
        & shape |?~ Avatar

  for_ [minBound .. maxBound] $ \s -> do
    ui $ List (conf & size |?~ s) $ do
      ui $ ListItem (def & image ?~ animalImage "cow") $ do
        ui $ ListHeader $ text "Charlie"
      ui $ ListItem (def & image ?~ animalImage "turtle") $ do
        ui $ ListHeader $ text "Tammy"
      ui $ ListItem (def & image ?~ animalImage "bear") $ do
        ui $ ListHeader $ text "Betty"
    ui $ Divider $ def & hidden |~ True
  |]

  ui $ Example "Floated" (def
    & subtitle ?~ text "A list can be floated")
    [example|
  let animalImage animal = Image ("images/animals/" <> animal <> ".png") $ def
        & shape |?~ Avatar

  ui $ List (def & floated |?~ LeftFloated) $ do

    ui $ ListItem (def & image ?~ animalImage "cow") $ do
      ui $ ListHeader $ text "Charlie"

    ui $ ListItem (def & image ?~ animalImage "turtle") $ do
      ui $ ListHeader $ text "Tammy"

    ui $ ListItem (def & image ?~ animalImage "bear") $ do
      ui $ ListHeader $ text "Betty"

  ui $ List (def & floated |?~ RightFloated) $ do

    ui $ ListItem (def & image ?~ animalImage "cow") $ do
      ui $ ListHeader $ text "Charlie"

    ui $ ListItem (def & image ?~ animalImage "turtle") $ do
      ui $ ListHeader $ text "Tammy"

    ui $ ListItem (def & image ?~ animalImage "bear") $ do
      ui $ ListHeader $ text "Betty"
  |]

