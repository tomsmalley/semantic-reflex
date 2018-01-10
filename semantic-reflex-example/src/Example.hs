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
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-do-bind #-}

module Example where

import Control.Lens ((^.), (?~))
import Control.Monad (void)
import Data.Foldable (for_)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.SemanticUI
import Reflex.Dom.Core (text)
import Language.Javascript.JSaddle hiding ((!!))

import Example.QQ
import Example.Common

import Example.Section.Buttons (buttonSection)
import Example.Section.Checkbox (checkboxes)
import Example.Section.Dimmer (dimmers)
import Example.Section.Divider (dividers)
import Example.Section.Dropdown (dropdowns)
import Example.Section.Flag (flags)
import Example.Section.Header
import Example.Section.Icon (iconSection)
import Example.Section.Input (inputs)
import Example.Section.Label (labels)
import Example.Section.List (lists)
import Example.Section.Menu (menu)
import Example.Section.Message (messages)
import Example.Section.Transition (transitions)

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
    Nothing -> void $ consoleLog ("el does not exist" :: Text)
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


intro :: MonadWidget t m => m ()
intro = do
  pageHeader H2 def $ text "Introduction"
  paragraph $ do
    text "This library aims to provide a type safe Haskell wrapper around Semantic UI components, to allow easy construction of nice looking web applications in GHCJS. It is currently in early development and started as a fork of the "
    httpLink "https://github.com/reflex-frp/reflex-dom-semui" $
      text "reflex-dom-semui"
    text " library."
  paragraph $ text "This page serves to provide an example of the library and components in use. Examples are shown along with the code that generated them."

  pageHeader H3 def $ text "Overview"

  paragraph $ text "To avoid having lots of unnecessary dynamics in config types, we use the type:"
  hscode $(printDefinition oneline id ''Active)
  paragraph $ text "For the common use case of config values to 'pure value' (in the case of Active, this translates to Static), we also provide lenses:"
  hscode $(printDefinition oneline id '(|?~))
  hscode $(printDefinition oneline id '(|~))

putSections :: MonadWidget t m => [Section t m] -> m ()
putSections sections = do
  pb :: Event t () <- delay 0.1 =<< getPostBuild
  onLoadEvent <- performEvent $ liftJSM getLocationHash <$ pb
  performEvent_ $ liftJSM . scrollIntoView <$> fmapMaybe id onLoadEvent

  uiElement "div" (def
    & elConfigAttributes |~ ("id" =: "main")
    & elConfigClasses |~ "ui container") $ do

    rec

      -- Menu
      {-
      divClass "ui dividing right rail" $
        sticky def $ do
          pageHeader H4 def $ text "Components"
          --divClass "ui vertical following fluid accordion text menu" $ do

          let conf = def
                & vertical .~ True
                & fluid .~ True
                & textContent .~ True
                & value . event ?~ onLoadEvent

              renderItem (LinkedSection heading _ _)
                = MenuItem (toId heading) def $ staticText heading

          (selected, _) <- ui $ Menu conf $ mapM_ (ui_ . renderItem) sections

          performEvent_ $ fmap (\id -> do
            liftJSM $ setLocationHash id
            liftJSM $ scrollIntoView id
            ) $ fmapMaybe id $ updated selected
      -}

      -- Sections
      (contextEl, _) <- divClass' "context" $ do
        intro
        for_ sections $ \(LinkedSection heading subHeading child) -> do
          let hConf = def
                & headerDividing |~ True
                & style |~ Style ("margin-top" =: "3em")
                & attrs |~ ("id" =: toId heading)
          pageHeader H2 hConf $ do
            text heading
            subHeader subHeading
          child

  {-
    performEvent_ $ (void . liftJSM $ do
      o <- obj
      o <# ("offset" :: Text) $ (30 :: Int)
      o <# ("context" :: Text) $ _element_raw contextEl
      o <# ("observeChanges" :: Text) $ True
      jQuery (_element_raw stickyEl) ^. js1 ("sticky" :: Text) o) <$ pb
-}

    return ()

  where
    toId = T.intercalate "-" . T.words . T.toLower
    {-
    renderItems [] = MenuBase
    renderItems (LinkedSection heading _ _:rest)
      = MenuItem (toId heading) heading def $ renderItems rest
      -}

main :: JSM ()
main = catchJS $ mainWidget runWithLoader

runWithLoader :: MonadWidget t m => m ()
runWithLoader = do
  pb <- delay 0 =<< getPostBuild
  rec loadingDimmer pb'
      liftJSM syncPoint
      pb' <- fmap updated $ widgetHold blank $ main' <$ pb
  return ()

loadingDimmer :: MonadWidget t m => Event t () -> m ()
loadingDimmer evt =
  dimmer (def & dimmerPage .~ True & transition ?~ (def & transConfigEvent .~ (Transition Fade def <$ evt))) $
    divClass "ui huge text loader" $ text "Loading semantic-reflex docs..."

main' :: MonadWidget t m => m ()
main' = do

  segment (def & attrs |~ ("id" =: "masthead") & segmentVertical |~ True) $
    divClass "ui container" $ do
      let semanticLogo = Image "https://semantic-ui.com/images/logo.png" $ def
            & imageShape |?~ Rounded
      pageHeader H1 (def & headerImage ?~ semanticLogo) $ do
        text "Semantic UI for Reflex Dom"
        subHeader $ text "Documentation and examples"
      button (def & buttonElement ?~ LinkButton & buttonDisabled |~ True) $
        text "Hackage"
      button (def
        & buttonElement ?~ LinkButton
        & buttonColor |?~ Teal
        & attrs |~ ("href" =: "https://github.com/tomsmalley/semantic-reflex"))
        $ do
        icon "github" def
        text "GitHub"

  putSections
    [ inputs
    , dropdowns
    , menu
    , dimmers
    , buttonSection
    , checkboxes
    , dividers
    , flags
    , headers
    , iconSection
    , labels
    , lists
    , menu
    , messages
    , transitions
    ]

