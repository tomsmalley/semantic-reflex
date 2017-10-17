{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}

module Example.Section.Dimmer where

import Control.Lens
import Control.Monad ((<=<))
import Reflex.Dom.SemanticUI

import Example.QQ
import Example.Common

dimmers :: forall t m. MonadWidget t m => Section t m
dimmers = LinkedSection "Dimmer" (text "A dimmers hides distractions to focus attention on particular content") $ do

  hscode $ $(printDefinition id stripParens ''Dimmer)
  hscode $ $(printDefinition id stripParens ''DimmerConfig)

  ui_ $ PageHeader H3 def $ text "Types"

  ui_ $ Example "Dimmer" (def
    & subtitle ?~ text "A standard dimmer")
    [example|
  evt <- ui $ Button def $ text "Toggle"
  inv <- toggle False <=< ui $ Button def $ text "Invert"

  ui_ $ Segment (def & inverted .~ Dynamic inv) $ do
    ui_ $ Header def $ text "Some Content"
    paragraph $ text $ "Suspendisse hendrerit justo id dignissim maximus. Ut maximus eu arcu sit amet egestas. Aenean et dictum felis. Ut rhoncus ipsum non luctus scelerisque. Proin pellentesque sed mauris efficitur aliquet. Duis imperdiet pulvinar rhoncus. Fusce tempor sem aliquet, mattis turpis vitae, rutrum orci. Aliquam in metus volutpat mi commodo dignissim et sed magna."

    let conf = def
          & transition ?~ (def & event .~ (Transition Fade def <$ evt))
    ui_ $ Dimmer conf $ do
      divClass "ui text loader" $ text "Dimmed"
  |]

  ui_ $ Example "Content Dimmer" (def
    & subtitle ?~ text "A dimmer can have content")
    [example|
  evt <- ui $ Buttons def $ do
    on <- ui $ Button (def & icon |~ True) $ ui $ Icon "plus" def
    off <- ui $ Button (def & icon |~ True) $ ui $ Icon "minus" def
    return $ leftmost
      [ Transition Fade (def & direction ?~ In) <$ on
      , Transition Fade (def & direction ?~ Out) <$ off ]

  ui_ $ Segment def $ do
    ui_ $ Header def $ text "Some Content"
    paragraph $ text $ "Suspendisse hendrerit justo id dignissim maximus. Ut maximus eu arcu sit amet egestas. Aenean et dictum felis. Ut rhoncus ipsum non luctus scelerisque. Proin pellentesque sed mauris efficitur aliquet. Duis imperdiet pulvinar rhoncus. Fusce tempor sem aliquet, mattis turpis vitae, rutrum orci. Aliquam in metus volutpat mi commodo dignissim et sed magna."
    paragraph $ text $ "Fusce varius bibendum eleifend. Integer ut finibus risus. Nunc non condimentum lorem. Sed aliquam scelerisque maximus. In hac habitasse platea dictumst. Nam mollis orci sem, vel posuere mi sollicitudin molestie. Praesent at pretium ex. Proin condimentum lacus sit amet risus volutpat, vitae feugiat ante iaculis. Nullam id interdum diam, nec sagittis quam. Ut finibus dapibus nunc sed tincidunt. Vestibulum mi lorem, euismod ut molestie id, viverra vitae ex."

    let conf = def
          & transition ?~ (def & event .~ evt)
    ui_ $ Dimmer conf $ do
      divClass "content" $ divClass "center" $ do
        let headerConfig = def & icon ?~ Icon "heart" def
                         & iconHeader |~ True & inverted |~ True
        ui $ PageHeader H2 headerConfig $ do
          text "Dimmed Message!"
  |]

  ui_ $ Example "Page Dimmer" (def
    & subtitle ?~ text "A dimmer can be fixed to the page")
    [example|
  on <- ui $ Button def $ ui (Icon "plus" def) >> text "Show"
  let evt = Transition Fade (def & direction ?~ In) <$ on

  let conf = def
        & transition ?~ (def & initial .~ True & event .~ evt)
        & page .~ True

  ui_ $ Dimmer conf $ do
    divClass "content" $ divClass "center" $ do
      let headerConfig = def & icon ?~ Icon "mail" def
                        & iconHeader |~ True & inverted |~ True
      ui $ PageHeader H2 headerConfig $ do
        text "Dimmed Message!"
        ui $ SubHeader $ text "Dimmer sub-header"
  |]

  ui_ $ Example "Persitent Dimmer" (def
    & subtitle ?~ text "A dimmer can disable close on click")
    --[example|
      ("code", Left $ mdo
    on <- ui $ Button def $ ui (Icon "plus" def) >> text "Show"

    let evt = leftmost
          [ Transition Fade (def & direction ?~ In) <$ on
          , Transition Fade (def & direction ?~ Out) <$ close ]

        conf = def
          & transition ?~ (def & initial .~ True & event .~ evt)
          & page .~ True
          & closeOnClick |~ False

    close <- ui $ Dimmer conf $ do
      divClass "content" $ divClass "center" $ do
        let headerConfig = def & icon ?~ Icon "warning sign" def
                          & iconHeader |~ True & inverted |~ True
        ui $ PageHeader H2 headerConfig $ do
          text "Persistent Dimmer"
          ui $ SubHeader $ text "You can't dismiss me without clicking the button"

        ui $ Divider $ def & hidden |~ True

        ui $ Button def $ text "Dismiss"

    return ()
    )
--  |]

  ui_ $ Example "Dimmer Events" (def
    & subtitle ?~ text "A dimmer can respond to events")
    --[example|
      ("code", Left $ mdo

    let trans d = Transition SlideDown (def & direction ?~ d & duration .~ 0.3)
    let evt = leftmost
          [ trans In <$ domEvent Mouseenter e
          , trans Out <$ domEvent Mouseleave e ]

    let conf = def
          & transition ?~ (def & initial .~ True & event .~ evt)
          & closeOnClick |~ False

    (e, _) <- ui' $ ContentImage "images/animals/cat.png" (def & size |?~ Medium) $ do
      ui_ $ Dimmer conf $ do
        divClass "content" $ divClass "center" $ do
          ui $ PageHeader H2 (def & inverted |~ True) $ text "Title"
          ui $ Button (def & emphasis |?~ Primary) $ text "Add"
          ui $ Button def $ text "View"

    return ()
    )
--  |]

