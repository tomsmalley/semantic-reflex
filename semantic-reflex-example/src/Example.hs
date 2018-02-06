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

import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Lens ((^.), (?~))
import Control.Monad (void, (<=<))
import Data.Bool (bool)
import Data.Foldable (for_)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Text (Text)
import Reflex.Dom.SemanticUI
import Reflex.Dom.Core (text)
import Reflex.Dom.RouteWriter
import Reflex.Dom.NestedRoute
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
    , ("Progress", NotImplemented, Nothing)
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

intro :: forall t m. (RouteWriter t Text m, MonadWidget t m) => Section t m
intro = Section "Introduction" blank $ do
  paragraph $ do
    text "This library aims to provide a type safe Haskell wrapper around Semantic UI components, to allow easy construction of nice looking web applications in GHCJS. It is currently in early development and started as a fork of the "
    hyperlink "https://github.com/reflex-frp/reflex-dom-semui" $
      text "reflex-dom-semui"
    text " library, although it has since been completely rewritten to remove dependencies on external JavaScript."

  message (def & messageType |?~ InfoMessage) $
    paragraph $ do
      icon "announcement" def
      text "The implementation of this library does not depend on the Semantic UI or jQuery JavaScript libraries."

  paragraph $ text "This page serves to provide an example of the library and components in use. Examples are shown along with the code that generated them."

  pageHeader H3 def $ text "Progress"

  -- Progress chart
  segments def $ for_ (progressTable @t @m) $ \(Category name items) -> mdo

    open <- segment (def & segmentColor |?~ Teal) $ do
      (e, _) <- elAttr' "div" ("style" =: "cursor: pointer;") $ do
        icon' (bool "caret right" "caret down" <$> open) def
        text name
      toggle False $ domEvent Click e

    let mkTransition dir = Transition SlideDown $ def
          & transitionDirection ?~ dir
          & transitionDuration .~ 0.3
        actionConfig = def
          & actionEvent ?~ (mkTransition . bool Out In <$> updated open)
          & actionInitialDirection .~ Out

    table (def & tableAttached |?~ Attached & action ?~ actionConfig) $ do
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
    hscode $(printDefinition id id ''Active)
  paragraph $ text "This is used in the dropdown implementation."

-- | Convert a component name to a css id string
toId :: Text -> Text
toId = T.intercalate "-" . T.words . T.toLower

main :: JSM ()
main = mainWidget runWithLoader

runWithLoader :: MonadWidget t m => m ()
runWithLoader = do
  pb <- delay 0 =<< getPostBuild
  rec loadingDimmer pb'
      liftJSM syncPoint
      pb' <- fmap updated $ widgetHold blank $ app <$ pb
  return ()

loadingDimmer :: MonadWidget t m => Event t () -> m ()
loadingDimmer evt =
  dimmer (def & dimmerPage .~ True & action ?~
    (def & actionEvent ?~ (Transition Fade def <$ evt))) $
    divClass "ui huge text loader" $ text "Loading semantic-reflex docs..."

app :: forall t m. MonadWidget t m => m ()
app = runRouteWithPathInFragment $ fmap snd $ runRouteWriterT $ do

  -- Header
  segment (def & attrs |~ ("id" =: "masthead") & segmentVertical |~ True) $
    divClass "ui container" $ do
      let conf = def
            & headerImage ?~ semanticLogo
            & style |~ Style ("cursor" =: "pointer")
      (e, _) <- pageHeader' H1 conf $ do
        text "Semantic UI for Reflex Dom"
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
        & headerSub |~ True
        & headerIcon ?~ Icon "info" (def & iconColor |?~ Teal)
        & style |~ Style ("cursor" =: "pointer")
      categoryConfig isOpen = linkHeaderConfig
        & headerIcon ?~ Icon (bool "right angle" "down angle" <$> isOpen) def
      wrapper isOpen = def
        & style |~ Style ("margin-top" =: "1em")
        & action ?~ (def
          & actionEvent ?~ (Transition Instant def <$ updated isOpen)
          & actionInitialDirection .~ Out)

  -- Main content
  uiElement "div" mainConfig $ do

    -- Menu
    let s = Style ("overflow" =: "auto")
    rail RightRail (def & railDividing |~ True & style |~ s) $ sticky def $ do
      (e, _) <- pageHeader' H4 linkHeaderConfig $ text "Introduction"
      tellRoute $ [] <$ domEvent Click e
      for_ (progressTable @t @m) $ \Category {..} -> mdo
        (e, _) <- pageHeader' H4 (categoryConfig isOpen) $ text categoryName
        isOpen <- toggle False $ domEvent Click e
        uiElement "div" (wrapper isOpen) $
          menu (def & menuVertical |~ True & menuSecondary |~ True) $ do
            for_ categoryItems $ \(item, status, mWidget) -> do
              case mWidget of
                Nothing -> menuItem (def & menuItemDisabled |~ True) $ do
                  text $ item <> " (No examples)"
                Just _ -> do
                  (e, _) <- menuItem' def $ text item
                  tellRoute $ [toId item] <$ domEvent Click e
      divider $ def & dividerHidden |~ True

    withRoute $ \route -> case M.lookup route sections of
      Nothing -> localRedirect []
      Just (Section heading subHeading child) -> do
        pageHeader H2 (def & style |~ Style ("margin-top" =: "0.5em")) $ do
          text heading
          subHeader subHeading
        child

  -- Footer
  segment (def & segmentVertical |~ True
              & style |~ Style ("padding" =: "0")) blank
  segment (def & segmentVertical |~ True
              & segmentAligned |?~ CenterAligned) $ do
    buttons (def & buttonsSize |?~ Small) $ do
      hackageButton
      githubButton
    divider $ def & dividerHidden |~ True
    text $ "Animal icons courtesy of "
    let url = "https://www.creativetail.com/40-free-flat-animal-icons/"
    hyperlink url $ text "Creative Tail"

semanticLogo :: Reflex t => Image t
semanticLogo = Image url $ def & imageShape |?~ Rounded
  where url = "https://semantic-ui.com/images/logo.png"

githubButton :: MonadWidget t m => m ()
githubButton = void $ button conf $ do
  icon "github" def
  text "GitHub"
  where
    url = "https://github.com/tomsmalley/semantic-reflex"
    conf = def
      & buttonType .~ LinkButton & buttonColor |?~ Teal
      & attrs |~ ("href" =: url)

hackageButton :: MonadWidget t m => m ()
hackageButton = void $ button conf $ text "Hackage"
  where conf = def & buttonType .~ LinkButton & buttonDisabled |~ True