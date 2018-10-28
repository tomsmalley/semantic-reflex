{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE LambdaCase           #-}

module Example.Section.Dropdown where

import Control.Lens
import Control.Monad ((<=<), void, join)
import Data.Foldable (for_)
import Data.Text (Text)
import Reflex.Dom.SemanticUI
import Reflex.Dom.Core (text)
import Data.Map.Lazy (Map)
import Data.Semigroup ((<>))

import Example.QQ
import Example.Common
import Example.StateEnum
import Example.CountryEnum

import qualified Data.Map.Lazy as M
import qualified Data.Text as T

dropdowns :: MonadWidget t m => Section t m
dropdowns = Section "Dropdown" (simpleLink "https://semantic-ui.com/modules/dropdown.html") $ do

  hscode $(printDefinition id stripParens ''DropdownConfig)
  hscode $(printDefinition id stripParens ''SearchDropdownConfig)
  hscode $(printDefinition id stripParens ''Dropdown)

  pageHeader H3 def $ text "Dropdown"

  mkExample "Dropdown" (def
    & dynamic ?~ dynShowCode
    & subtitle ?~ text "A standard dropdown")
    [resetExample|
  \resetEvent -> do
    let conf = def { _dropdownConfig_placeholder = "State" }
    dropdown conf Nothing (Nothing <$ resetEvent) $ TaggedStatic $ M.fromList $
      ffor [minBound .. maxBound] $ \x -> (x, text (stateText x))
  |]

  mkExample "Unselectable Dropdown" (def
    & dynamic ?~ dynShowCode
    & subtitle ?~ text "An unselectable dropdown allows a user to reset the value by selecting it again")
    [resetExample|
  \resetEvent -> do
    let conf = def
          { _dropdownConfig_placeholder = "State"
          , _dropdownConfig_unselectable = True
          }
    dropdown conf Nothing (Nothing <$ resetEvent) $ TaggedStatic $ M.fromList $
      ffor [minBound .. maxBound] $ \x -> (x, text (stateText x))
  |]

  mkExample "Search Dropdown" (def
    & dynamic ?~ dynShowCode
    & subtitle ?~ text "A dropdown can be searchable")
    [resetExample|
  \resetEvent -> do
    let conf = def
          { _dropdownConfig_search = Just $ searchByToText countryText
          , _dropdownConfig_placeholder = "Country"
          }
    dropdown conf Nothing (Nothing <$ resetEvent) $ TaggedStatic $ M.fromList $
      ffor [minBound .. maxBound] $
        \x -> (x, flag (pure $ T.toLower $ tshow x) def >> text (countryText x))
  |]

  mkExample "Custom Dropdown" (def
    & dynamic ?~ dynShowCode
    & subtitle ?~ text "A dropdown can use a different wrapper")
    [resetExample|
  \resetEvent -> do
    let conf = def
          { _dropdownConfig_search = Just $ searchByToText countryText
          , _dropdownConfig_placeholder = "Country"
          , _dropdownConfig_selection = pure False
          }
    dropdownWithWrapper (\f -> label' (f def)) conf Nothing (Nothing <$ resetEvent) $
      TaggedStatic $ M.fromList $ ffor [minBound .. maxBound] $
        \x -> (x, flag (pure $ T.toLower $ tshow x) def >> text (countryText x))
  |]

  mkExample "Search Dropdown (allow additions)" (def
    & dynamic ?~ dynCode
    & subtitle ?~ text "A searchable dropdown can allow the user to enter a new value")
    [resetExample|
  \resetEvent -> do
    let conf = def
          { _dropdownConfig_search = Just $ (searchByToText countryText)
            { _searchDropdownConfig_allowAdditions = True }
          , _dropdownConfig_placeholder = "Country"
          }
    d <- dropdown conf Nothing (Nothing <$ resetEvent) $ TaggedStatic $ M.fromList $
      ffor [minBound .. maxBound] $
        \x -> (x, flag (pure $ T.toLower $ tshow x) def >> text (countryText x))
    pure $ ffor (allowAdditionsValue d) $ \x -> case x of
      Left t -> "Some other place, " <> t
      Right (Just a) -> "A known place, " <> countryText a
      Right Nothing -> "Choose a place!"
  |]

  mkExample "Identity Dropdown" (def
    & dynamic ?~ dynShowCode
    & subtitle ?~ text "A dropdown can always have a value")
    [resetExample|
  \resetEvent -> do
    let conf = def { _dropdownConfig_placeholder = "State" }
    dropdown conf (Identity NY) (Identity NY <$ resetEvent) $ TaggedStatic $ M.fromList $
      ffor [minBound .. maxBound] $ \x -> (x, text (stateText x))
  |]

  mkExample "Text Dropdown" (def
    & dynamic ?~ dynShowCode
    & subtitle ?~ text "A text input that can provide suggestions in a dropdown")
    [resetExample|
  \resetEvent -> do
    textDropdown def "" ("" <$ resetEvent) $ TaggedStatic ["one", "two", "three"]
  |]

  mkExample "Static or Dynamic Dropdown" (def
    & dynamic ?~ dynCode
    & subtitle ?~ text "A dropdown with lots of items. Note that the dynamic list takes a few times longer to load than the static list.")
    [resetExample|
  \resetEvent -> do
    on <- toggle True <=< button def $ text "Toggle"

    let items = [1 .. 1000] :: [Int]
        render = M.fromList [ (i, text $ tshow i) | i <- items ]

    e <- dyn $ ffor on $ \o -> case o of
      True -> dropdown (def & dropdownConfig_placeholder |~ "Static") Nothing never $
        TaggedStatic render
      False -> dropdown (def & dropdownConfig_placeholder |~ "Dynamic") Nothing never $
        TaggedDynamic $ pure render
    join <$> holdDyn (pure Nothing) (value <$> e)
  |]

