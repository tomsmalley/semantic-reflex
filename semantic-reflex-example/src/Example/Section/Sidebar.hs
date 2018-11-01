{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}

module Example.Section.Sidebar where

import Control.Lens
import Control.Monad ((<=<))
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (for)
import Reflex.Dom.SemanticUI
import Reflex.Dom.Core (text)

import Example.QQ
import Example.Common

sidebars :: forall t m. MonadWidget t m => Section t m
sidebars = Section "Sidebar" (text "A sidebar hides additional content beside a page.") $ do

  hscode $ $(printDefinition id stripParens ''SidebarConfig)

  hscode $ $(printDefinition oneline stripParens ''SidebarTransition)
  hscode $ $(printDefinition oneline stripParens ''SidebarWidth)
  hscode $ $(printDefinition oneline stripParens ''Side)

  mkExample "Sidebar" def [example|
  pageHeader H5 def $ text "Side"
  side <- buttonMenu Side_Left [minBound..maxBound]
  pageHeader H5 def $ text "Width"
  width <- buttonMenu SidebarWidth_Medium [minBound..maxBound]
  pageHeader H5 def $ text "Animations"
  transition <- buttonMenu SidebarTransition_Overlay [minBound..maxBound]
  pageHeader H5 def $ text "Other options"
  dimming <- toggleButton $ text "Dimming"
  closeOnClick <- toggleButton $ text "Close on click"
  direction <- buttons def $ do
    toggle <- button def $ text "Toggle"
    hide <- button def $ text "Hide"
    show' <- button def $ text "Show"
    pure $ leftmost [Nothing <$ toggle, Just Out <$ hide, Just In <$ show']

  sidebar side Out direction
    (def
      & sidebarConfig_transition .~ transition
      & sidebarConfig_dimming .~ dimming
      & sidebarConfig_closeOnClick .~ closeOnClick
      & sidebarConfig_width .~ width
    )
    (\f -> segment (f def))
    (\f -> menu (f $ def & menuConfig_inverted |~ True & menuConfig_vertical |~ True) $ menuItem def $ text "Menu")
    $ segment (def & segmentConfig_basic |~ True) $ do
        header def $ text "Some Content"
        paragraph $ text $ "Suspendisse hendrerit justo id dignissim maximus. Ut maximus eu arcu sit amet egestas. Aenean et dictum felis. Ut rhoncus ipsum non luctus scelerisque. Proin pellentesque sed mauris efficitur aliquet. Duis imperdiet pulvinar rhoncus. Fusce tempor sem aliquet, mattis turpis vitae, rutrum orci. Aliquam in metus volutpat mi commodo dignissim et sed magna."
  |]

  return ()

buttonMenu :: (Show a, Ord a, UI t m) => a -> [a] -> m (Dynamic t a)
buttonMenu ini as = buttons def $ mdo
  selected <- holdDyn ini $ leftmost es
  let selectedDemux = demux selected
  es <- for as $ \a -> do
    e <- selectableButton (demuxed selectedDemux a) $ text $ T.tail $ T.dropWhile (/= '_') $ tshow a
    pure $ a <$ e
  pure selected

toggleButton :: UI t m => m () -> m (Dynamic t Bool)
toggleButton content = mdo
  dimmingClick <- selectableButton dimming content
  dimming <- toggle False dimmingClick
  pure dimming

selectableButton :: UI t m => Dynamic t Bool -> m () -> m (Event t ())
selectableButton selected = button $ def
  { _buttonConfig_emphasis = Dyn $ ffor selected $ \case
    True -> Just Primary
    False -> Nothing
  }
