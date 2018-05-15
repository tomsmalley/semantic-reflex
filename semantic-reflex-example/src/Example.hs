{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-do-bind #-}

module Example where

import Control.Monad (void, (<=<))
import Data.Bool (bool)
import Data.Foldable (for_)
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Reflex.Dom.SemanticUI
import Reflex.Dom.Core (text)
import Reflex.Dom.Routing.Writer
import Reflex.Dom.Routing.Nested
import Language.Javascript.JSaddle hiding ((!!))

import qualified Data.Map as M
import qualified Data.Text as T

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
import Example.Section.Message (messages)
import Example.Section.Progress (progressSection)
import Example.Section.RadioGroup (radioGroups)
import Example.Section.Transition (transitions)

data Category t m = Category
  { categoryName :: Text
  , categoryItems :: [(Text, Status, Maybe (Section t m))]
  }
data Status = Implemented | PartiallyImplemented | NotImplemented deriving Show

-- | A log of the implementation progress
progressTable :: MonadWidget t m => [Category t m]
progressTable =
  [ Category "Elements"
    [ ("Button", Implemented, Just buttonSection)
    , ("Container", PartiallyImplemented, Nothing)
    , ("Divider", Implemented, Just dividers)
    , ("Flag", Implemented, Just flags)
    , ("Header", Implemented, Just headers)
    , ("Icon", Implemented, Just iconSection)
    , ("Image", Implemented, Nothing)
    , ("Input", Implemented, Just inputs)
    , ("Label", Implemented, Just labels)
    , ("List", Implemented, Just lists)
    , ("Loader", NotImplemented, Nothing)
    , ("Rail", Implemented, Nothing)
    , ("Reveal", NotImplemented, Nothing)
    , ("Segment", Implemented, Nothing)
    , ("Step", NotImplemented, Nothing)
    ]
  , Category "Collections"
    [ ("Breadcrumb", NotImplemented, Nothing)
    , ("Form", PartiallyImplemented, Nothing)
    , ("Grid", NotImplemented, Nothing)
    , ("Menu", NotImplemented, Nothing)
    , ("Message", Implemented, Just messages)
    , ("Table", PartiallyImplemented, Nothing)
    ]
  , Category "Views"
    [ ("Advertisement", NotImplemented, Nothing)
    , ("Card", NotImplemented, Nothing)
    , ("Comment", NotImplemented, Nothing)
    , ("Feed", NotImplemented, Nothing)
    , ("Item", NotImplemented, Nothing)
    , ("Statistic", NotImplemented, Nothing)
    ]
  , Category "Modules"
    [ ("Accordion", NotImplemented, Nothing)
    , ("Checkbox", Implemented, Just checkboxes)
    , ("Dimmer", Implemented, Just dimmers)
    , ("Dropdown", PartiallyImplemented, Just dropdowns)
    , ("Embed", NotImplemented, Nothing)
    , ("Modal", NotImplemented, Nothing)
    , ("Nag", NotImplemented, Nothing)
    , ("Popup", NotImplemented, Nothing)
    , ("Progress", Implemented, Just progressSection)
    , ("Rating", NotImplemented, Nothing)
    , ("Search", NotImplemented, Nothing)
    , ("Shape", NotImplemented, Nothing)
    , ("Sidebar", NotImplemented, Nothing)
    , ("Sticky", PartiallyImplemented, Nothing)
    , ("Tab", NotImplemented, Nothing)
    , ("Transition", Implemented, Just transitions)
    ]
  , Category "Behaviors"
    [ ("API", NotImplemented, Nothing)
    , ("Form Validation", NotImplemented, Nothing)
    , ("Visibility", NotImplemented, Nothing)
    ]
  ]

progressProgress :: forall t m. MonadWidget t m => m (Progress t m)
progressProgress = progress (pure $ Range 0 vMax) (pure v) $ def
  & progressConfig_color |?~ Teal
  & progressConfig_bar ?~ PercentageBar
  where
    categories = progressTable :: [Category t m]
    v = sum $ concatMap (fmap (\(_,i,_) -> implementedNum i) . categoryItems) categories
    vMax = sum $ map ((*2) . length . categoryItems) categories
    implementedNum = \case
      NotImplemented -> 0
      PartiallyImplemented -> 1
      Implemented -> 2

intro :: forall t m. (RouteWriter t Text m, MonadWidget t m) => Section t m
intro = Section "Introduction" blank $ do
  paragraph $ do
    text "This library aims to provide a type safe Haskell wrapper around Semantic UI components, to allow easy construction of nice looking web applications in GHCJS. It is currently in early development and started as a fork of the "
    hyperlink "https://github.com/reflex-frp/reflex-dom-semui" $
      text "reflex-dom-semui"
    text " library, although it has since been completely rewritten to remove dependencies on external JavaScript."

  message (def & messageConfig_type |?~ InfoMessage) $
    paragraph $ do
      icon "announcement" def
      text "The implementation of this library does not depend on the Semantic UI or jQuery JavaScript libraries."

  paragraph $ text "This page serves to provide an example of the library and components in use. Examples are shown along with the code that generated them."

  pageHeader H3 def $ text "Progress"

  void progressProgress

  -- Progress chart
  segments def $ for_ (progressTable @t @m) $ \(Category name items) -> mdo

    open <- segment (def & segmentConfig_color |?~ Teal) $ do
      (e, _) <- elAttr' "div" ("style" =: "cursor: pointer") $ do
        icon' (Dyn $ bool "caret right" "caret down" <$> open) def
        text name
      toggle False $ domEvent Click e

    let mkTransition dir = Transition SlideDown $ def
          & transitionConfig_direction ?~ dir
          & transitionConfig_duration .~ 0.3
        actionConfig = def
          & action_event ?~ (mkTransition . bool Out In <$> updated open)
          & action_initialDirection .~ Out

    table (def & tableConfig_attached |?~ Attached & action ?~ actionConfig) $ do
      thead $ tr $ do
        th $ text "Feature"
        th $ text "Status"
      tbody $ for_ items $ \(item, status, mWidget) -> tr $ do
        td $ case mWidget of
          Nothing -> text item
          Just _ -> hyperlink ("#" <> toId item) $ text item
        case status of
          Implemented -> elClass "td" "positive" $ text "Implemented"
          NotImplemented -> elClass "td" "negative" $ text "Not implemented"
          PartiallyImplemented -> elClass "td" "warning" $ text "Partially implemented"

  pageHeader H3 def $ text "Notes"

  paragraph $ text "For the common use case of config values to 'pure value', there are lenses:"
  paragraph $ do
    hscode $(printDefinition oneline id '(|?~))
    hscode $(printDefinition oneline id '(|~))

  paragraph $ text "In some cases (e.g. dropdowns) we want to write one function which optimises the common case of having a static list of items. For this, see the type:"
  paragraph $ do
    hscode $(printDefinition id id ''ActiveType)
    hscode $(printDefinition id id ''TaggedActive)
  paragraph $ text "This is used in the dropdown implementation."

-- | Convert a component name to a css id string
toId :: Text -> Text
toId = T.intercalate "-" . T.words . T.toLower

main :: JSM ()
main = mainWidget runWithLoader

testApp :: MonadWidget t m => m ()
testApp = do
  tog <- toggle True <=< button def $ text "Toggle"
  dyn $ ffor tog $ \case
    False -> for_ [1 :: Int ..2000] $ \i ->
      button (def & buttonConfig_color .~ Dyn (pure Nothing)) $
        text $ "Dynamic " <> tshow i
    True -> for_ [1 :: Int ..2000] $ \i -> button def $ text $ "Static " <> tshow i
  pure ()

runWithLoader :: MonadWidget t m => m ()
runWithLoader = do
  pb <- delay 0 =<< getPostBuild
  rec loadingDimmer pb'
      liftJSM syncPoint
      pb' <- fmap updated $ widgetHold blank $ app <$ pb
  return ()

loadingDimmer :: MonadWidget t m => Event t () -> m ()
loadingDimmer evt =
  dimmer (def & dimmerConfig_page .~ True & action ?~
    (def & action_event ?~ (Transition Fade def <$ evt))) $
    divClass "ui huge text loader" $ text "Loading semantic-reflex docs..."

app :: forall t m. MonadWidget t m => m ()
app = runRouteWithPathInFragment $ fmap snd $ runRouteWriterT $ do

  -- Header
  segment (def & attrs |~ ("id" =: "masthead") & segmentConfig_vertical |~ True) $
    divClass "ui container" $ do
      let conf = def
            & headerConfig_preContent ?~ semanticLogo
            & style |~ Style "cursor: pointer"
      (e, _) <- pageHeader' H1 conf $ do
        text "Semantic UI for Reflex-DOM"
        subHeader $ text "Documentation and examples"
      tellRoute $ [] <$ domEvent Click e
      hackageButton
      githubButton

  let sections = M.insert Nothing intro $ M.fromList
        $ mapMaybe (\(name, _, mSection) -> (,) (Just $ toId name) <$> mSection)
        $ concatMap categoryItems progressTable
      mainConfig =  def
        & elConfigAttributes |~ ("id" =: "main")
        & elConfigClasses |~ "ui container"
      linkHeaderConfig = def
        & headerConfig_sub |~ True
        & headerConfig_preContent ?~ icon "info" (def & iconConfig_color |?~ Teal)
        & style |~ Style "cursor: pointer"
      categoryConfig isOpen = linkHeaderConfig
        & headerConfig_preContent ?~ icon (Dyn $ bool "right angle" "down angle" <$> isOpen) def
      wrapper isOpen = def
        & style |~ Style "margin-top: 1em"
        & action ?~ (def
          & action_event ?~ (Transition Instant def <$ updated isOpen)
          & action_initialDirection .~ Out)

  -- Main content
  ui "div" mainConfig $ do

    -- Menu
    let s = Style "overflow: auto"
    rail RightRail (def & railConfig_dividing |~ True & style |~ s) $ sticky def $ do
      (e, _) <- pageHeader' H4 linkHeaderConfig $ text "Introduction"
      tellRoute $ [] <$ domEvent Click e
      for_ (progressTable @t @m) $ \Category {..} -> mdo
        (e, _) <- pageHeader' H4 (categoryConfig isOpen) $ text categoryName
        isOpen <- toggle False $ domEvent Click e
        ui "div" (wrapper isOpen) $
          menu (def & menuConfig_vertical |~ True & menuConfig_secondary |~ True) $ do
            for_ categoryItems $ \(item, status, mWidget) -> do
              case mWidget of
                Nothing -> menuItem (def & menuItemConfig_disabled |~ True) $ do
                  text $ item <> " (No examples)"
                Just _ -> do
                  (e, _) <- menuItem' def $ text item
                  tellRoute $ [toId item] <$ domEvent Click e
      divider $ def & dividerConfig_hidden |~ True

    withRoute $ \route -> case M.lookup route sections of
      Nothing -> localRedirect []
      Just (Section heading subHeading child) -> do
        pageHeader H2 (def & style |~ Style "margin-top: 0.5em") $ do
          text heading
          subHeader subHeading
        child

  -- Footer
  segment (def & segmentConfig_vertical |~ True
              & style |~ Style "padding: 0") blank
  segment (def & segmentConfig_vertical |~ True
              & segmentConfig_aligned |?~ CenterAligned) $ do
    buttons (def & buttonsConfig_size |?~ Small) $ do
      hackageButton
      githubButton
    divider $ def & dividerConfig_hidden |~ True
    text $ "Animal icons courtesy of "
    let url = "https://www.creativetail.com/40-free-flat-animal-icons/"
    hyperlink url $ text "Creative Tail"

semanticLogo :: MonadWidget t m => m ()
semanticLogo = image (def & imageConfig_shape |?~ Rounded) $ Left $ Img url def
  where url = "https://semantic-ui.com/images/logo.png"

githubButton :: MonadWidget t m => m ()
githubButton = void $ button conf $ do
  icon "github" def
  text "GitHub"
  where
    url = "https://github.com/tomsmalley/semantic-reflex"
    conf = def
      & buttonConfig_type .~ LinkButton & buttonConfig_color |?~ Teal
      & attrs |~ ("href" =: url)

hackageButton :: MonadWidget t m => m ()
hackageButton = void $ button conf $ text "Hackage"
  where conf = def & buttonConfig_type .~ LinkButton & buttonConfig_disabled |~ True
