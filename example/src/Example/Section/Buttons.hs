{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}

module Example.Section.Buttons where

import Control.Lens
import Control.Monad ((<=<))
import Data.Bool (bool)
import Data.Foldable (for_)
import Data.Semigroup ((<>))
import Reflex.Dom.SemanticUI

import Example.QQ
import Example.Common

buttons :: forall t m. MonadWidget t m => Section t m
buttons = LinkedSection "Button" (text "") $ do

  paragraph $ text ""

  hscode $(printDefinition id stripParens ''Button)
  hscode $(printDefinition id stripParens ''DivButton)
  hscode $(printDefinition id stripParens ''ButtonConfig)

  ui_ $ PageHeader H3 def $ text "Types"

  ui_ $ Example "Button" (def & subtitle ?~ text "A standard button")
    [example|
  ui_ $ Button def $ text "Button"
  ui_ $ DivButton def $ text "DivButton"
  |]

  ui_ $ Example "Emphasis" (def
    & subtitle ?~ text "A button can have a different level of emphasis")
    [example|
  ui_ $ Button (def & emphasis |?~ Primary) $ text "Primary"
  ui_ $ Button (def & emphasis |?~ Secondary) $ text "Secondary"
  -- Tertiary has no special styles for buttons
  ui_ $ Button (def & emphasis |?~ Tertiary) $ text "Tertiary"
  ui_ $ Button def $ text "Normal"
  |]

  ui_ $ Example "Animated" (def
    & subtitle ?~ text "A button animate to show hidden content")
    [example|
  let rightAnim = def & content .~ ui_ (Icon "right arrow" def)
  ui_ $ Button (def & animated ?~ rightAnim) $ text "Next"

  let shopAnim = def & content .~ text "Shop" & animatedType |~ VerticalAnimated
  ui_ $ Button (def & animated ?~ shopAnim) $ ui_ $ Icon "shop" def

  let priceAnim = def & content .~ text "£12.99 a month" & animatedType |~ AnimatedFade
  ui_ $ Button (def & animated ?~ priceAnim) $ text "Sign-up for a Pro account"
  |]

  ui_ $ Example "Labeled" (def
    & subtitle ?~ text "A button can have a label")
    [example|
  ui_ $ LabeledButton def $ do
    ui_ $ Button (def & color |?~ Red) $ do
      ui_ $ Icon "heart" def
      text "Like"
    ui_ $ Label (def & basic |~ True & color |?~ Red & pointing |?~ LeftPointing) $ text "256"

  ui_ $ LabeledButton (def & labeled |~ LeftLabeled) $ do
    ui_ $ Label (def & basic |~ True & color |?~ Blue & pointing |?~ RightPointing) $ text "38"
    ui_ $ Button (def & color |?~ Blue & basic |~ True) $ do
      ui_ $ Icon "fork" def
      text "Fork"

  ui_ $ LabeledButton (def & labeled |~ LeftLabeled) $ do
    ui_ $ Label (def & basic |~ True) $ text "1,048"
    ui_ $ Button (def & icon |~ True) $ do
      ui_ $ Icon "fork" def
  |]

  ui_ $ Example "Icon" (def
    & subtitle ?~ text "A button can have only an icon")
    [example|
  ui_ $ Button (def & icon |~ True) $ do
    ui_ $ Icon "cloud" def
  |]

  ui_ $ Example "Labeled Icon" (def
    & subtitle ?~ text "A button can use an icon as a label")
    [example|
  ui_ $ Button (def & labeledIcon |?~ LeftLabeled) $ do
    ui_ $ Icon "pause" def
    text "Pause"
  ui_ $ Button (def & labeledIcon |?~ RightLabeled) $ do
    ui_ $ Icon "play" def
    text "Play"
  |]

  ui_ $ Example "Basic" (def
    & subtitle ?~ text "A basic button is less pronounced")
    [example|
  ui_ $ Button (def & basic |~ True) $ do
    ui_ $ Icon "user" def
    text "Add Friend"
  ui_ $ Button (def & basic |~ True & emphasis |?~ Primary) $ text "Primary"
  ui_ $ Button (def & basic |~ True & emphasis |?~ Secondary) $ text "Secondary"
  ui_ $ Button (def & basic |~ True & positive |?~ Positive) $ text "Positive"
  ui_ $ Button (def & basic |~ True & positive |?~ Negative) $ text "Negative"
  |]

  ui_ $ Example "Inverted" (def
    & subtitle ?~ text "A button can be formatted to appear on dark backgrounds")
    [example|
  ui_ $ Segment (def & inverted |~ True) $ do
    for_ [minBound .. maxBound] $ \c -> do
      let conf = def & inverted |~ True & color |?~ c
                     & style |~ Style ("margin-bottom" =: "0.75em")
      ui_ $ Button conf $ text $ Static $ tshow c
    ui_ $ Divider def
    for_ [minBound .. maxBound] $ \c -> do
      let conf = def & basic |~ True & inverted |~ True & color |?~ c
                     & style |~ Style ("margin-bottom" =: "0.75em")
      ui_ $ Button conf $ text $ Static $ "Basic " <> tshow c
  |]

  ui_ $ Example "Disabled" (def
    & subtitle ?~ text "A button can show it is currently unable to be interacted with")
    [example|
  ui_ $ Button (def & disabled |~ True) $ do
    ui_ $ Icon "user" def
    text "Followed"
  |]

  ui_ $ Example "Loading" (def
    & subtitle ?~ text "A button can show a loading indicator")
    [example|
  start <- ui $ Button def $ text "Start"
  stop <- ui $ Button def $ text "Stop"
  isLoading <- holdDyn True $ leftmost [True <$ start, False <$ stop]
  ui_ $ Divider (def & hidden |~ True)
  ui_ $ Button (def & loading .~ Dynamic isLoading) $ text "Loaded"
  ui_ $ Button (def & basic |~ True & loading .~ Dynamic isLoading) $ text "Loaded"
  ui_ $ Button (def & emphasis |?~ Primary & loading .~ Dynamic isLoading) $ text "Loaded"
  ui_ $ Button (def & emphasis |?~ Secondary & loading .~ Dynamic isLoading) $ text "Loaded"
  |]

  ui_ $ Example "Social" (def
    & subtitle ?~ text "A button can be formatted in the style of a social website")
    [example|
  for_ [minBound .. maxBound] $ \s -> do
    let conf = def & social |?~ s & style |~ Style ("margin-bottom" =: "0.75em")
    ui_ $ Button conf $ do
     ui_ $ Icon (Static $ toClassText s) def
     text $ Static $ tshow s
  |]

  ui_ $ Example "Size" (def
    & subtitle ?~ text "A button can have different sizes")
    [example|
  for_ [minBound .. maxBound] $ \s -> do
    ui_ $ Button (def & size |?~ s) $ text $ Static $ tshow s
  |]

  ui_ $ Example "Floated" (def
    & subtitle ?~ text "A button can be floated")
    [example|
  ui_ $ Button (def & floated |?~ LeftFloated) $ text "Left Floated"
  ui_ $ Button (def & floated |?~ RightFloated) $ text "Right Floated"
  |]

  ui_ $ Example "Color" (def
    & subtitle ?~ text "A button can have different colors")
    [example|
  for_ [minBound .. maxBound] $ \c -> do
    let conf = def & color |?~ c & style |~ Style ("margin-bottom" =: "0.75em")
    ui_ $ Button conf $ text $ Static $ tshow c
  |]

  ui_ $ Example "Compact" (def
    & subtitle ?~ text "A button can reduce its padding")
    [example|
  ui_ $ Button (def & compact |~ True) $ text "Hold"
  ui_ $ Button (def & compact |~ True & icon |~ True) $ ui_ $ Icon "pause" def
  ui_ $ Button (def & compact |~ True & labeledIcon |?~ LeftLabeled) $ do
    ui_ $ Icon "pause" def
    text "Pause"
  |]

  ui_ $ Example "Toggle" (def
    & subtitle ?~ text "A button can be toggled"
    & dynamic ?~ dynCode
    & inbetween ?~ (ui_ $ Message (def & messageType |?~ InfoMessage)
      $ paragraph $ do
        ui_ $ Icon "info circle" def
        text "Semantic UI has some sneaky javascript that gives state to anything with a 'toggle' class. Here you must deal with state explicitly."))
    ([str|
  \resetEvent -> do
    rec let conf = def & emphasis .~ Dynamic (bool (Just Primary) Nothing <$> on)
        on <- toggle False <=< ui_ $ Button conf $ text "Toggle"
    return on
  |], Left $ do
    rec let conf = def & emphasis .~ Dynamic (bool Nothing (Just Primary) <$> on)
        on <- toggle False <=< ui $ Button conf $ text "Toggle"
    return on
    )

  ui_ $ Example "Fluid" (def
    & subtitle ?~ text "A button can fill its container")
    [example|
  ui_ $ Button (def & fluid |~ True) $ text "Fits container"
  |]

  ui_ $ Example "Circular" (def
    & subtitle ?~ text "A button can be circular")
    [example|
  ui_ $ Button (def & circular |~ True & icon |~ True) $ ui_ $ Icon "settings" def
  for_ [minBound .. maxBound] $ \s -> do
    let conf = def & social |?~ s & circular |~ True & icon |~ True
    ui_ $ Button conf $ ui_ $ Icon (Static $ toClassText s) def
  |]

  ui_ $ Example "Attached" (def
    & subtitle ?~ text "A button can be attached to the top or bottom of other content"
    & inbetween ?~ upstreamIssue 5851 "Attached 'Button' elements are incorrectly sized, use 'DivButton' until this is fixed upstream.")
    [example|
  ui_ $ DivButton (def & attached |?~ Vertically TopAttached) $ text "Top"
  ui_ $ Segment (def & attached |?~ Attached) $ text "Segment"
  ui_ $ DivButton (def & attached |?~ Vertically BottomAttached) $ text "Bottom"
  |]

  ui_ $ PageHeader H2 def $ text "Buttons"

  ui_ $ Example "Buttons" (def
    & subtitle ?~ text "Buttons can exist together as a group")
    [resetExample|
  \resetEvent -> ui_ $ Buttons def $ do
    one <- ui $ Button def $ text "One"
    two <- ui $ Button def $ text "Two"
    three <- ui $ Button def $ text "Three"
    holdDyn Nothing $ leftmost
      [ Just (1 :: Int) <$ one, Just 2 <$ two, Just 3 <$ three, Nothing <$ resetEvent ]
  |]

  ui_ $ Example "Icon Buttons" (def
    & subtitle ?~ text "Buttons groups can show groups of icons")
    [example|
  ui_ $ Buttons (def & icon |~ True) $ do
    ui_ $ Button def $ ui_ $ Icon "align left" def
    ui_ $ Button def $ ui_ $ Icon "align center" def
    ui_ $ Button def $ ui_ $ Icon "align right" def
    ui_ $ Button def $ ui_ $ Icon "align justify" def

  ui_ $ Buttons (def & icon |~ True) $ do
    ui_ $ Button def $ ui_ $ Icon "bold" def
    ui_ $ Button def $ ui_ $ Icon "italic" def
    ui_ $ Button def $ ui_ $ Icon "underline" def
  |]

  ui_ $ Example "Conditionals" (def
    & subtitle ?~ text "Buttons groups can contain conditionals")
    [example|
  ui_ $ Buttons def $ do
    ui_ $ Button def $ text "un"
    ui_ $ Conditional $ def & dataText |?~ "ou"
    ui_ $ Button (def & positive |?~ Positive) $ text "deux"
  ui_ $ Divider def
  ui_ $ Buttons def $ do
    ui_ $ Button def $ text "Cancel"
    ui_ $ Conditional def
    ui_ $ Button (def & positive |?~ Positive) $ text "Save"
  |]

  ui_ $ Example "Attached group" (def
    & subtitle ?~ text "A group of buttons can be attached to the top or bottom of other content")
    [example|
  ui_ $ Buttons (def & attached |?~ TopAttached & width |?~ Two) $ do
    ui_ $ Button def $ text "One"
    ui_ $ Button def $ text "Two"
  ui_ $ Segment (def & attached |?~ Attached) $ text "Segment"
  ui_ $ Buttons (def & attached |?~ BottomAttached & width |?~ Two) $ do
    ui_ $ Button def $ text "One"
    ui_ $ Button def $ text "Two"
  |]

  ui_ $ Example "Horizontally Attached" (def
    & subtitle ?~ text "Buttons can be attached to the left or right of other content")
    [example|
  ui_ $ Button (def & attached |?~ Horizontally LeftAttached) $ text "Left"
  ui_ $ Button (def & attached |?~ Horizontally RightAttached) $ text "Right"
  |]

  ui_ $ Example "Vertical Buttons" (def
    & subtitle ?~ text "Groups can be formatted to appear vertically")
    [example|
  ui_ $ Buttons (def & vertical |~ True) $ do
    ui_ $ Button def $ text "Feed"
    ui_ $ Button def $ text "Messages"
    ui_ $ Button def $ text "Events"
    ui_ $ Button def $ text "Photos"
  |]

  ui_ $ Example "Labeled Icon Buttons" (def
    & subtitle ?~ text "Groups can be formatted as labeled icons")
    [example|
  ui_ $ Buttons (def & vertical |~ True & labeledIcon |~ True) $ do
    ui_ $ Button def $ do
      ui_ $ Icon "pause" def
      text "Pause"
    ui_ $ Button def $ do
      ui_ $ Icon "play" def
      text "Play"
    ui_ $ Button def $ do
      ui_ $ Icon "shuffle" def
      text "Shuffle"
  |]

  ui_ $ Example "Mixed Group" (def
    & subtitle ?~ text "Groups can be formatted to use multiple button types together")
    [example|
  ui_ $ Buttons def $ do
    ui_ $ Button (def & labeledIcon |?~ LeftLabeled) $ do
      ui_ $ Icon "left chevron" def
      text "Back"
    ui_ $ Button def $ do
      ui_ $ Icon "stop" def
      text "Stop"
    ui_ $ Button (def & labeledIcon |?~ RightLabeled) $ do
      ui_ $ Icon "right chevron" def
      text "Forward"
  |]

  ui_ $ Example "Equal Width" (def
    & subtitle ?~ text "Groups can have their widths divided evenly")
    [example|
  ui_ $ Buttons (def & width |?~ Five) $ do
    ui_ $ Button def $ text "Overview"
    ui_ $ Button def $ text "Specs"
    ui_ $ Button def $ text "Warranty"
    ui_ $ Button def $ text "Reviews"
    ui_ $ Button def $ text "Support"

  ui_ $ Divider $ def & hidden |~ True

  ui_ $ Buttons (def & width |?~ Three) $ do
    ui_ $ Button def $ text "Overview"
    ui_ $ Button def $ text "Specs"
    ui_ $ Button def $ text "Support"
  |]

  ui_ $ Example "Colored Buttons" (def
    & subtitle ?~ text "Buttons can share a color")
    [example|
  ui_ $ Buttons (def & color |?~ Blue) $ do
    ui_ $ Button def $ text "One"
    ui_ $ Button def $ text "Two"
    ui_ $ Button def $ text "Three"
  |]

  ui_ $ Example "Compact Buttons" (def
    & subtitle ?~ text "Buttons can be compact")
    [example|
  ui_ $ Buttons (def & compact |~ True) $ do
    ui_ $ Button def $ text "One"
    ui_ $ Button def $ text "Two"
    ui_ $ Button def $ text "Three"
  |]

  ui_ $ Example "Floated Buttons" (def
    & subtitle ?~ text "Button groups can be floated")
    [example|
  ui_ $ Buttons (def & floated |?~ LeftFloated) $ do
    ui_ $ Button def $ text "One"
    ui_ $ Button def $ text "Two"
    ui_ $ Button def $ text "Three"

  ui_ $ Buttons (def & floated |?~ RightFloated) $ do
    ui_ $ Button def $ text "One"
    ui_ $ Button def $ text "Two"
    ui_ $ Button def $ text "Three"
  |]

  ui_ $ Example "Basic Buttons" (def
    & subtitle ?~ text "Buttons can be less pronounced")
    [example|
  ui_ $ Buttons (def & basic |~ True) $ do
    ui_ $ Button def $ text "One"
    ui_ $ Button def $ text "Two"
    ui_ $ Button def $ text "Three"

  ui_ $ Divider $ def & hidden |~ True

  ui_ $ Buttons (def & vertical |~ True & basic |~ True) $ do
    ui_ $ Button def $ text "One"
    ui_ $ Button def $ text "Two"
    ui_ $ Button def $ text "Three"
  |]

  ui_ $ Example "Group Sizes" (def
    & subtitle ?~ text "Buttons can share a size")
    [example|
  ui_ $ Buttons (def & size |?~ Large) $ do
    ui_ $ Button def $ text "One"
    ui_ $ Button def $ text "Two"
    ui_ $ Button def $ text "Three"

  ui_ $ Divider $ def & hidden |~ True

  ui_ $ Buttons (def & size |?~ Small & basic |~ True & icon |~ True) $ do
    ui_ $ Button def $ ui_ $ Icon "file" def
    ui_ $ Button def $ ui_ $ Icon "save" def
    ui_ $ Button def $ ui_ $ Icon "upload" def
    ui_ $ Button def $ ui_ $ Icon "download" def

  ui_ $ Divider $ def & hidden |~ True

  ui_ $ Buttons (def & size |?~ Large) $ do
    ui_ $ Button def $ text "One"
    ui_ $ Conditional def
    ui_ $ Button def $ text "Three"
  |]

  return ()

