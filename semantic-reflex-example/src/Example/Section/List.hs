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
import Reflex.Dom.Core (text)

import Example.QQ
import Example.Common

data Files a = File a | Folder a [Files a]

lists :: MonadWidget t m => Section t m
lists = Section "List" (text "A list is used to group related content. " >> simpleLink "https://semantic-ui.com/elements/list.html") $ do

  hscode $(printDefinition id stripParens ''ListConfig)
  hscode $(printDefinition id stripParens ''ListItemConfig)

  mkExample "List" (def
    & subtitle ?~ text "A list groups related content")
    [example|
  list def $ for_ ["Apples", "Pears", "Oranges"] $ listItem def . text
  |]

  mkExample "" def
    [example|
  list def $ do
    listItem (def & listItemPreContent ?~ icon "users" def) $ text "Semantic UI"
    listItem (def & listItemPreContent ?~ icon "marker" def) $ text "New York, NY"
    listItem (def & listItemPreContent ?~ icon "mail" def) $ simpleLink "jack@semantic-ui.com"
    listItem (def & listItemPreContent ?~ icon "linkify" def) $ simpleLink "semantic-ui.com"
  |]

  mkExample "" def
    [example|
  list (def & listDivided |~ True & listRelaxed |?~ Relaxed) $ do

    let repos = [ ("Semantic-Org/Semantic-UI", 10 :: Int)
                , ("Semantic-Org/Semantic-UI-Docs", 22)
                , ("Semantic-Org/Semantic-UI-Meteor", 34) ]

    let github = icon "github" $ def
          & iconSize |?~ Large & classes |~ "middle aligned"

    for_ repos $ \(name, time) -> listItem (def & listItemPreContent ?~ github) $ do
      listHeader $ text name
      listDescription $ text $ "Updated " <> tshow time <> " mins ago"
  |]

  mkExample "" def
    [example|
  let
    -- In the absence of local data declarations:
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

    listFiles [] = blank
    listFiles fs = list def $ for_ fs $ \f -> case f of
      File (name, description) -> listItem (def & listItemPreContent ?~ icon "file" def) $ do
        listHeader $ text name
        listDescription $ text description
      Folder (name, description) fs' -> listItem (def & listItemPreContent ?~ icon "folder" def) $ do
        listHeader $ text name
        listDescription $ text description
        listFiles fs'

  listFiles files
  |]

  mkExample "Bulleted" (def
    & subtitle ?~ text "A list can mark items with a bullet")
    [example|
  list (def & listType |?~ Bulleted) $ do
    listItem def $ text "Gaining Access"
    listItem def $ text "Inviting Friends"
    listItem def $ do
      text "Benefits"
      list def $ do
        listItem def $ hyperlink "" $ text "Link to somewhere"
        listItem def $ text "Rebates"
        listItem def $ text "Discounts"
    listItem def $ text "Warranty"
  |]

  mkExample "" def
    [example|
  list (def & listType |?~ Bulleted & listHorizontal |~ True) $ do
    listItem def $ hyperlink "#" $ text "About Us"
    listItem def $ hyperlink "#" $ text "Sitemap"
    listItem def $ hyperlink "#" $ text "Contact"
  |]

  mkExample "Ordered" (def
    & subtitle ?~ text "A list can be ordered numerically")
    [example|
  list (def & listType |?~ Ordered) $ do
    listItem def $ hyperlink "#" $ text "Getting Started"
    listItem def $ hyperlink "#" $ text "Introduction"
    listItem def $ do
      hyperlink "#" $ text "Languages"
      list def $ do
        listItem def $ hyperlink "#" $ text "HTML"
        listItem def $ hyperlink "#" $ text "Javascript"
        listItem def $ hyperlink "#" $ text "CSS"
    listItem def $ hyperlink "#" $ text "Review"
  |]

  mkExample "Value" (def
    & subtitle ?~ text "A list item can have a custom value"
    & inbetween ?~ upstreamIssue 5911 "The data-value attribute is currently ignored for <div> list items.")
    [example|
  list (def & listType |?~ Ordered) $ do
    listItem (def & attrs |~ "data-value" =: "*") $ text "Signing Up"
    listItem (def & attrs |~ "data-value" =: "*") $ text "User Benefits"
    listItem (def & attrs |~ "data-value" =: "*") $ do
      text "User Types"
      list def $ do
        listItem (def & attrs |~ "data-value" =: "-") $ text "Admin"
        listItem (def & attrs |~ "data-value" =: "-") $ text "Power User"
        listItem (def & attrs |~ "data-value" =: "-") $ text "Regular User"
    listItem (def & attrs |~ "data-value" =: "*") $ text "Deleting Your Account"
  |]

  mkExample "Link" (def
    & subtitle ?~ text "A list can be specially formatted for navigation links")
    [example|
  list (def & listLink |~ True) $ do
    let conf = def & listItemElement .~ ListItemLink
    listItem (conf & classes |~ "active") $ text "Home"
    listItem conf $ text "About"
    listItem conf $ text "Jobs"
    listItem conf $ text "Team"
  |]

  mkExample "Icon" (def
    & subtitle ?~ text "A list item can contain an icon")
    [example|
  list def $ do
    let conf = def & listItemElement .~ ListItemLink
    listItem (conf & listItemPreContent ?~ icon "help" def) $ do
      listHeader $ text "Floated Icon"
      listDescription $ text "This text will always have a left margin to make sure it sits alongside your icon"

    listItem (conf & listItemPreContent ?~ icon "right triangle" def) $ do
      listHeader $ text "Icon Alignment"
      listDescription $ text "Floated icons are by default top aligned"

    listItem (def & listItemPreContent ?~ icon "help" def) $ text "Inline Text"
  |]

  mkExample "Image" (def
    & subtitle ?~ text "A list item can contain an image")
    [example|
  list def $ do

    let animalImage animal = image (def & imageShape |?~ Avatar) $
          Left $ Img ("images/animals/" <> animal <> ".png") def

    listItem (def & listItemPreContent ?~ animalImage "fox") $ do
      listHeader $ hyperlink "#" $ text "Fiona"
      listDescription $ text "Last seen rummaging through the bins just now."

    listItem (def & listItemPreContent ?~ animalImage "elephant") $ do
      listHeader $ hyperlink "#" $ text "Ethan"
      listDescription $ text "Last seen playing in the water 2 hours ago."

    listItem (def & listItemPreContent ?~ animalImage "monkey") $ do
      listHeader $ hyperlink "#" $ text "Marcus"
      listDescription $ text "Last seen swinging on his tyre yesterday."

    listItem (def & listItemPreContent ?~ animalImage "giraffe") $ do
      listHeader $ hyperlink "#" $ text "Ginny"
      listDescription $ text "Last seen nibbling on a tree 3 days ago."

    listItem (def & listItemPreContent ?~ animalImage "spider") $ do
      listHeader $ hyperlink "#" $ text "Stanley"
      listDescription $ text "Last seen spooking people a week ago."
  |]

  mkExample "Link" (def
    & subtitle ?~ text "A list can contain links")
    [example|
  list def $ do
    let conf = def & listItemElement .~ ListItemLink
    listItem conf $ text "What is a FAQ?"
    listItem conf $ text "Who is our user?"
    listItem conf $ text "Where is our office located?"
  |]

  mkExample "Header" (def
    & subtitle ?~ text "A list item can contain a header")
    [example|
  list def $ do

    listItem def $ do
      listHeader $ text "Manchester"
      text "A lovely city"

    listItem def $ do
      listHeader $ text "Sheffield"
      text "Also quite a lovely city"

    listItem def $ do
      listHeader $ text "London"
      text "Sometimes can be a lovely city"

    listItem def $ do
      listHeader $ text "Bristol"
      text "What a lovely city"
  |]

  mkExample "Description" (def
    & subtitle ?~ text "A list item can contain a description")
    [example|
  list def $ do

    listItem (def & listItemPreContent ?~ icon "map marker" def) $ do
      listHeader $ hyperlink "#" $ text "Krolewskie Jadlo"
      listDescription $ text "An excellent polish restaurant, quick delivery and hearty, filling meals."

    listItem (def & listItemPreContent ?~ icon "map marker" def) $ do
      listHeader $ hyperlink "#" $ text "Sapporo Haru"
      listDescription $ text "Greenpoint's best choice for quick and delicious sushi."
  |]

  mkExample "Horizontal" (def
    & subtitle ?~ text "A list can be formatted to have items appear horizontally")
    [example|
  list (def & listHorizontal |~ True) $ do

    let animalImage animal = image (def & imageShape |?~ Avatar) $
          Left $ Img ("images/animals/" <> animal <> ".png") def

    listItem (def & listItemPreContent ?~ animalImage "eagle") $ do
      listHeader $ text "Eddie"
      text "Top Contributor"

    listItem (def & listItemPreContent ?~ animalImage "tiger") $ do
      listHeader $ text "Tommy"
      text "Admin"

    listItem (def & listItemPreContent ?~ animalImage "kangaroo") $ do
      listHeader $ text "Katie"
      text "Top Rated User"
  |]

  mkExample "Inverted" (def
    & subtitle ?~ text "A list can be inverted to appear on a dark background")
    [example|
  segment (def & segmentInverted |~ True) $
    list (def & listInverted |~ True
              & listRelaxed |?~ Relaxed
              & listDivided |~ True) $ do

      listItem def $ do
        listHeader $ text "Snickerdoodle"
        text "An excellent companion"

      listItem def $ do
        listHeader $ text "Poodle"
        text "A poodle, it's pretty basic"

      listItem def $ do
        listHeader $ text "Paulo"
        text "He's also a dog"
  |]

  mkExample "Selection" (def
    & subtitle ?~ text "A selection list formats list items as possible choices")
    [example|
  list (def & listSelection |~ True & listAligned |?~ ListMiddle) $ do

    let animalImage animal = image (def & imageShape |?~ Avatar) $
          Left $ Img ("images/animals/" <> animal <> ".png") def

    listItem (def & listItemPreContent ?~ animalImage "horse") $
      listHeader $ text "Helen"

    listItem (def & listItemPreContent ?~ animalImage "chicken") $
      listHeader $ text "Christian"

    listItem (def & listItemPreContent ?~ animalImage "duck") $
      listHeader $ text "Daniel"
  |]

  mkExample "Animated" (def
    & subtitle ?~ text "A list can animate to set the current item apart from the list")
    [example|
  list (def & listAnimated |~ True & listAligned |?~ ListMiddle) $ do

    let animalImage animal = image (def & imageShape |?~ Avatar) $
          Left $ Img ("images/animals/" <> animal <> ".png") def

    listItem (def & listItemPreContent ?~ animalImage "horse") $
      listHeader $ text "Helen"

    listItem (def & listItemPreContent ?~ animalImage "chicken") $
      listHeader $ text "Christian"

    listItem (def & listItemPreContent ?~ animalImage "duck") $
      listHeader $ text "Daniel"
  |]

  mkExample "Relaxed" (def
    & subtitle ?~ text "A list can relax its padding to provide more negative space")
    [example|
  list def $ do
    listItem def $ text "One"
    listItem def $ text "Two"
    listItem def $ text "Three"

  divider def

  list (def & listRelaxed |?~ Relaxed) $ do
    listItem def $ text "One"
    listItem def $ text "Two"
    listItem def $ text "Three"

  divider def

  list (def & listRelaxed |?~ VeryRelaxed) $ do
    listItem def $ text "One"
    listItem def $ text "Two"
    listItem def $ text "Three"

  divider def

  list (def & listHorizontal |~ True) $ do
    listItem def $ text "One"
    listItem def $ text "Two"
    listItem def $ text "Three"

  divider def

  list (def & listRelaxed |?~ Relaxed & listHorizontal |~ True) $ do
    listItem def $ text "One"
    listItem def $ text "Two"
    listItem def $ text "Three"

  divider def

  list (def & listRelaxed |?~ VeryRelaxed & listHorizontal |~ True) $ do
    listItem def $ text "One"
    listItem def $ text "Two"
    listItem def $ text "Three"
  |]

  mkExample "Divided" (def
    & subtitle ?~ text "A list can show divisions between items")
    [example|
  list (def & listDivided |~ True & listAligned |?~ ListMiddle) $ do

    let animalImage animal = image (def & imageShape |?~ Avatar) $
          Left $ Img ("images/animals/" <> animal <> ".png") def

    listItem (def & listItemPreContent ?~ animalImage "cow") $
      listHeader $ text "Charlie"

    listItem (def & listItemPreContent ?~ animalImage "turtle") $
      listHeader $ text "Tammy"

    listItem (def & listItemPreContent ?~ animalImage "bear") $
      listHeader $ text "Betty"

  divider $ def & dividerHidden |~ True

  list (def & listDivided |~ True & listHorizontal |~ True) $ do
    listItem def $ text "About Us"
    listItem def $ text "Contact"
    listItem def $ text "Support"
  |]

  mkExample "Celled" (def
    & subtitle ?~ text "A list can divide its items into cells")
    [example|
  list (def & listCelled |~ True & listAligned |?~ ListMiddle) $ do

    let animalImage animal = image (def & imageShape |?~ Avatar) $
          Left $ Img ("images/animals/" <> animal <> ".png") def

    listItem (def & listItemPreContent ?~ animalImage "cow") $ do
      listHeader $ text "Charlie"
      text "A cow"

    listItem (def & listItemPreContent ?~ animalImage "turtle") $ do
      listHeader $ text "Tammy"
      text "A turtle"

    listItem (def & listItemPreContent ?~ animalImage "bear") $ do
      listHeader $ text "Betty"
      text "A bear"

  divider $ def & dividerHidden |~ True

  list (def & listCelled |~ True & listHorizontal |~ True) $ do
    listItem def $ text "About Us"
    listItem def $ text "Contact"
    listItem def $ text "Support"
  |]

  mkExample "Size" (def
    & subtitle ?~ text "A list can vary in size")
    [example|
  let conf = def & listHorizontal |~ True & listAligned |?~ ListMiddle
      animalImage animal = image (def & imageShape |?~ Avatar) $
        Left $ Img ("images/animals/" <> animal <> ".png") def

  for_ [minBound .. maxBound] $ \s -> do
    list (conf & listSize |?~ s) $ do
      listItem (def & listItemPreContent ?~ animalImage "cow") $ do
        listHeader $ text "Charlie"
      listItem (def & listItemPreContent ?~ animalImage "turtle") $ do
        listHeader $ text "Tammy"
      listItem (def & listItemPreContent ?~ animalImage "bear") $ do
        listHeader $ text "Betty"
    divider $ def & dividerHidden |~ True
  |]

  mkExample "Floated" (def
    & subtitle ?~ text "A list can be floated")
    [example|
  let animalImage animal = image (def & imageShape |?~ Avatar) $
        Left $ Img ("images/animals/" <> animal <> ".png") def

  list (def & listFloated |?~ LeftFloated) $ do

    listItem (def & listItemPreContent ?~ animalImage "cow") $ do
      listHeader $ text "Charlie"

    listItem (def & listItemPreContent ?~ animalImage "turtle") $ do
      listHeader $ text "Tammy"

    listItem (def & listItemPreContent ?~ animalImage "bear") $ do
      listHeader $ text "Betty"

  list (def & listFloated |?~ RightFloated) $ do

    listItem (def & listItemPreContent ?~ animalImage "cow") $ do
      listHeader $ text "Charlie"

    listItem (def & listItemPreContent ?~ animalImage "turtle") $ do
      listHeader $ text "Tammy"

    listItem (def & listItemPreContent ?~ animalImage "bear") $ do
      listHeader $ text "Betty"
  |]

  return ()

