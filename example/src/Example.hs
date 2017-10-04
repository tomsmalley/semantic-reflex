{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures       #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-do-bind #-}

module Example (example) where

import GHC.Tuple
import Control.Lens
import Control.Monad ((<=<), void, when, forM_, join, replicateM_, guard)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.SemanticUI
import Language.Javascript.JSaddle hiding ((!!))

import Example.StateEnum
import Example.CountryEnum
import Example.QQ
import Example.Common

import Example.Section.Checkbox (checkboxes)
import Example.Section.Dropdown (dropdowns)
import Example.Section.Flag (flags)
import Example.Section.Header
import Example.Section.Icon (icons)
import Example.Section.Label (labels)
import Example.Section.Menu (menu)
import Example.Section.Message (messages)
import Example.Section.RadioGroup (radioGroups)

data Favourite
  = Haskell
  | Semantic
  | Reflex
  deriving (Eq, Show)

scrollIntoView :: Text -> JSM ()
scrollIntoView id = do
  document <- jsg ("document" :: Text)
  o <- obj
  o <# ("block" :: Text) $ ("start" :: Text)
  o <# ("behavior" :: Text) $ ("smooth" :: Text)
  mEl :: Maybe JSVal <- fromJSVal =<< document ^. js1 ("getElementById" :: Text) id
  case mEl of
    Nothing -> consoleLog ("el does not exist" :: Text) >> return ()
    Just el -> void $ el ^. js1 ("scrollIntoView" :: Text) o

getLocationHash :: JSM (Maybe Text)
getLocationHash = do
  document <- jsg ("document" :: Text)
  mhash :: Maybe Text <- fromJSVal =<< document ^. js ("location" :: Text) ^. js ("hash" :: Text)
  return $ T.stripPrefix "#" =<< mhash

setLocationHash :: Text -> JSM ()
setLocationHash hash = do
  history <- jsg ("history" :: Text)
  void $ history ^. js3 ("pushState" :: Text) jsNull jsNull hash'
  where
    hash' = if "#" `T.isPrefixOf` hash then hash else "#" <> hash


putSections :: MonadWidget t m => [Section m] -> m ()
putSections sections = do
  pb :: Event t () <- delay 0.1 =<< getPostBuild
  onLoadEvent <- performEvent $ liftJSM getLocationHash <$ pb
  performEvent_ $ liftJSM . scrollIntoView <$> fmapMaybe id onLoadEvent

  elAttr "div" ("id" =: "main" <> "class" =: "ui container") $ do
    -- Menu
    (stickyEl, _) <- divClass "ui dividing right rail" $ do
      elAttr' "div" ("class" =: "ui sticky") $ do

        ui $ Header H4 (text "Components") def
        --divClass "ui vertical following fluid accordion text menu" $ do
        (selected, HNil) <- ui $ Menu
          (renderItems sections) $ def
            & vertical .~ True
            & fluid .~ True
            & textContent .~ True
            & setValue .~ onLoadEvent
        performEvent_ $ fmap (\id -> do
          liftJSM $ setLocationHash id
          liftJSM $ scrollIntoView id
          ) $ fmapMaybe id $ updated selected

    -- Sections
    (contextEl, _) <- el' "div" $ do

      divClass "intro" $ do
        ui $ Header H2 (text "Introduction") def
        el "p" $ do
          text "This library aims to provide a type safe Haskell wrapper around Semantic UI components, to allow easy construction of nice looking web applications in GHCJS. It is currently in early development and started as a fork of the "
          elAttr "a" ("href" =: "https://github.com/reflex-frp/reflex-dom-semui") $ text "reflex-dom-semui"
          text " library."
        el "p" $ text "This page serves to provide an example of the library and components in use. Examples are shown along with the code that generated them."

        ui $ Header H3 (text "Overview") def
        el "p" $ text "The library exposes components in the form of data types. The convention is to have a record with all parts required to specify a component, with the last being a config type that contains the optional or unnecessary parts. All of the component types have overloaded field lenses so they can be modified concisely."
        el "p" $ do
          text "Components can be rendered using the function "
          hsCodeInline $(printDefinition oneline id 'ui)
          text "."

        el "p" $ text "To avoid having lots of unnecessary dynamics in config types, we use the type:"
        hscode $(printDefinition oneline id ''Active)
        el "p" $ text "For the common use case of config values to 'pure value' (in the case of Active, this translates to Static), we also provide lenses:"
        hscode $(printDefinition oneline id '(|?~))
        hscode $(printDefinition oneline id '(|~))

      forM_ sections $ \(LinkedSection heading subHeading child) -> do
        ui $ Header H2 (text heading) $ def
          & dividing |~ True
          & attributes |~ ("id" =: toId heading <> "style" =: "margin-top: 3em")
          & subHeader ?~ subHeading
        child

    performEvent_ $ (void . liftJSM $ do
      o <- obj
      o <# ("offset" :: Text) $ (30 :: Int)
      o <# ("context" :: Text) $ _element_raw contextEl
      o <# ("observeChanges" :: Text) $ True
      jQuery (_element_raw stickyEl) ^. js1 ("sticky" :: Text) o) <$ pb

    return ()

  where
    toId = T.intercalate "-" . T.words . T.toLower
    renderItems [] = MenuBase
    renderItems (LinkedSection heading _ _:rest)
      = MenuItem (toId heading) heading def $ renderItems rest

example :: JSM ()
example = mainWidget $ do

  elAttr "div" ("id" =: "masthead" <> "class" =: "ui vertical segment") $ do
    divClass "ui container" $ do
      let semanticLogo = Image "https://semantic-ui.com/images/logo.png" $ def
            & size |?~ Massive & rounded |?~ Rounded
      ui $ Header H1 (text "Semantic UI for Reflex Dom") $ def
        & image .~ AlwaysRender semanticLogo
        & subHeader ?~ text "Documentation and examples"
      elAttr "a" ("class" =: "ui disabled button" <> "href" =: "") $ text "Hackage"
      elAttr "a" ("class" =: "ui teal button"
               <> "href" =: "https://github.com/tomsmalley/reflex-dom-semui") $ do
        ui $ Icon "github" $ def
        text "GitHub"
      return ()

  putSections [ checkboxes, dropdowns, flags, headers, icons, labels, menu, messages, radioGroups ]

