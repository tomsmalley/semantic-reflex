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
import Reflex.Dom.Core (text)

import Example.QQ
import Example.Common

buttonSection :: forall t m. MonadWidget t m => Section t m
buttonSection = Section "Button" (text "") $ do

  paragraph $ text ""

  hscode $(printDefinition id stripParens ''ButtonConfig)

  pageHeader H3 def $ text "Types"

  mkExample "Button" (def & subtitle ?~ text "A standard button")
    [example|
  button def $ text "Button"
  button (def & buttonConfig_type .~ DivButton) $ text "Div Button"
  button (def & buttonConfig_type .~ LinkButton) $ text "Link Button"
  |]

  mkExample "Emphasis" (def
    & subtitle ?~ text "A button can have a different level of emphasis")
    [example|
  button (def & buttonConfig_emphasis |?~ Primary) $ text "Primary"
  button (def & buttonConfig_emphasis |?~ Secondary) $ text "Secondary"
  -- Tertiary has no special styles for buttons
  button (def & buttonConfig_emphasis |?~ Tertiary) $ text "Tertiary"
  button def $ text "Normal"
  |]

  mkExample "Animated" (def
    & subtitle ?~ text "A button animate to show hidden content")
    [example|
  let rightAnim = def & animatedButton_hiddenContent .~ icon "right arrow" def
  button (def & buttonConfig_animated ?~ rightAnim) $ text "Next"

  let shopAnim = def
        & animatedButton_hiddenContent .~ text "Shop"
        & animatedButton_type |~ VerticalAnimated
  button (def & buttonConfig_animated ?~ shopAnim) $ icon "shop" def

  let priceAnim = def
        & animatedButton_hiddenContent .~ text "£12.99 a month"
        & animatedButton_type |~ AnimatedFade
  button (def & buttonConfig_animated ?~ priceAnim) $ text "Sign-up for a Pro account"
  |]

  mkExample "Labeled" (def
    & subtitle ?~ text "A button can have a label")
    [example|
  labeledButton def $ do
    button (def & buttonConfig_color |?~ Red) $ do
      icon "heart" def
      text "Like"
    let conf = def
          & labelConfig_basic |~ True
          & labelConfig_color |?~ Red
          & labelConfig_pointing |?~ LeftPointing
    label conf $ text "256"

  labeledButton (def & labeledButtonConfig_side |~ LeftLabeled) $ do
    let conf = def
          & labelConfig_basic |~ True
          & labelConfig_color |?~ Blue
          & labelConfig_pointing |?~ RightPointing
    label conf $ text "38"
    button (def & buttonConfig_color |?~ Blue & buttonConfig_basic |~ True) $ do
      icon "fork" def
      text "Fork"

  labeledButton (def & labeledButtonConfig_side |~ LeftLabeled) $ do
    label (def & labelConfig_basic |~ True) $ text "1,048"
    button (def & buttonConfig_icon |~ True) $ do
      icon "fork" def
  |]

  mkExample "Icon" (def
    & subtitle ?~ text "A button can have only an icon")
    [example|
  button (def & buttonConfig_icon |~ True) $ do
    icon "cloud" def
  |]

  mkExample "Labeled Icon" (def
    & subtitle ?~ text "A button can use an icon as a label")
    [example|
  button (def & buttonConfig_labeledIcon |?~ LeftLabeled) $ do
    icon "pause" def
    text "Pause"
  button (def & buttonConfig_labeledIcon |?~ RightLabeled) $ do
    icon "play" def
    text "Play"
  |]

  mkExample "Basic" (def
    & subtitle ?~ text "A basic button is less pronounced")
    [example|
  button (def & buttonConfig_basic |~ True) $ do
    icon "user" def
    text "Add Friend"
  button (def & buttonConfig_basic |~ True & buttonConfig_emphasis |?~ Primary) $
    text "Primary"
  button (def & buttonConfig_basic |~ True & buttonConfig_emphasis |?~ Secondary) $
    text "Secondary"
  button (def & buttonConfig_basic |~ True & buttonConfig_positive |?~ Positive) $
    text "Positive"
  button (def & buttonConfig_basic |~ True & buttonConfig_positive |?~ Negative) $
    text "Negative"
  |]

  mkExample "Inverted" (def
    & subtitle ?~ text "A button can be formatted to appear on dark backgrounds")
    [example|
  segment (def & segmentConfig_inverted |~ True) $ do
    for_ [minBound .. maxBound] $ \c -> do
      let conf = def & buttonConfig_inverted |~ True
                     & buttonConfig_color |?~ c
                     & style |~ Style "margin-bottom: 0.75em"
      button conf $ text $ tshow c
    divider def
    for_ [minBound .. maxBound] $ \c -> do
      let conf = def & buttonConfig_basic |~ True
                     & buttonConfig_inverted |~ True
                     & buttonConfig_color |?~ c
                     & style |~ Style "margin-bottom: 0.75em"
      button conf $ text $ "Basic " <> tshow c
  |]

  mkExample "Disabled" (def
    & subtitle ?~ text "A button can show it is currently unable to be interacted with")
    [example|
  button (def & buttonConfig_disabled |~ True) $ do
    icon "user" def
    text "Followed"
  |]

  mkExample "Loading" (def
    & subtitle ?~ text "A button can show a loading indicator")
    [example|
  start <- button def $ text "Start"
  stop <- button def $ text "Stop"
  isLoading <- holdDyn True $ leftmost [True <$ start, False <$ stop]
  divider (def & dividerConfig_hidden |~ True)
  button (def & buttonConfig_loading .~ Dyn isLoading) $ text "Loaded"
  button (def & buttonConfig_basic |~ True
              & buttonConfig_loading .~ Dyn isLoading) $ text "Loaded"
  button (def & buttonConfig_emphasis |?~ Primary
              & buttonConfig_loading .~ Dyn isLoading) $ text "Loaded"
  button (def & buttonConfig_emphasis |?~ Secondary
              & buttonConfig_loading .~ Dyn isLoading) $ text "Loaded"
  |]

  mkExample "Social" (def
    & subtitle ?~ text "A button can be formatted in the style of a social website")
    [example|
  for_ [minBound .. maxBound] $ \s -> do
    let conf = def & buttonConfig_social |?~ s
                   & style |~ Style "margin-bottom: 0.75em"
    button conf $ do
     icon (pure $ toClassText s) def
     text $ tshow s
  |]

  mkExample "Size" (def
    & subtitle ?~ text "A button can have different sizes")
    [example|
  for_ [minBound .. maxBound] $ \s -> do
    button (def & buttonConfig_size |?~ s) $ text $ tshow s
  |]

  mkExample "Floated" (def
    & subtitle ?~ text "A button can be floated")
    [example|
  button (def & buttonConfig_floated |?~ LeftFloated) $ text "Left Floated"
  button (def & buttonConfig_floated |?~ RightFloated) $ text "Right Floated"
  |]

  mkExample "Color" (def
    & subtitle ?~ text "A button can have different colors")
    [example|
  for_ [minBound .. maxBound] $ \c -> do
    let conf = def & buttonConfig_color |?~ c
                   & style |~ Style "margin-bottom: 0.75em"
    button conf $ text $ tshow c
  |]

  mkExample "Compact" (def
    & subtitle ?~ text "A button can reduce its padding")
    [example|
  button (def & buttonConfig_compact |~ True) $ text "Hold"
  button (def & buttonConfig_compact |~ True
              & buttonConfig_icon |~ True) $ icon "pause" def
  button (def & buttonConfig_compact |~ True
              & buttonConfig_labeledIcon |?~ LeftLabeled) $ do
    icon "pause" def
    text "Pause"
  |]

  mkExample "Toggle" (def
    & subtitle ?~ text "A button can be toggled"
    & dynamic ?~ dynCode
    & inbetween ?~ (message (def & messageConfig_type |?~ InfoMessage)
      $ paragraph $ do
        icon "info circle" def
        text "Semantic UI has some sneaky javascript that gives state to anything with a 'toggle' class. Here you must deal with state explicitly."))
    ([str|
  \resetEvent -> do
    rec let conf = def & buttonConfig_emphasis .~ Dyn (bool Nothing (Just Primary) <$> on)
        on <- toggle False <=< button conf $ text "Toggle"
    return on
  |], Left $ do
    rec let conf = def & buttonConfig_emphasis .~ Dyn (bool Nothing (Just Primary) <$> on)
        on <- toggle False <=< button conf $ text "Toggle"
    return on
    )

  mkExample "Fluid" (def
    & subtitle ?~ text "A button can fill its container")
    [example|
  button (def & buttonConfig_fluid |~ True) $ text "Fits container"
  |]

  mkExample "Circular" (def
    & subtitle ?~ text "A button can be circular")
    [example|
  button (def & buttonConfig_circular |~ True & buttonConfig_icon |~ True) $
    icon "settings" def
  for_ [minBound .. maxBound] $ \s -> do
    let conf = def & buttonConfig_social |?~ s
                   & buttonConfig_circular |~ True
                   & buttonConfig_icon |~ True
    button conf $ icon (pure $ toClassText s) def
  |]

  mkExample "Attached" (def
    & subtitle ?~ text "A button can be attached to the top or bottom of other content"
    & inbetween ?~ upstreamIssue 5851 "Attached 'Button' elements are incorrectly sized, use an alternate tag until this is fixed upstream.")
    [example|
  let conf = def & buttonConfig_type .~ DivButton
  button (conf & buttonConfig_attached |?~ Vertically TopAttached) $ text "Top"
  segment (def & segmentConfig_attached |?~ Attached) $ text "Segment"
  button (conf & buttonConfig_attached |?~ Vertically BottomAttached) $ text "Bottom"
  |]

  pageHeader H2 def $ text "Buttons"

  mkExample "Buttons" (def
    & subtitle ?~ text "Buttons can exist together as a group")
    [resetExample|
  \resetEvent -> buttons def $ do
    one <- button def $ text "One"
    two <- button def $ text "Two"
    three <- button def $ text "Three"
    holdDyn Nothing $ leftmost
      [ Just (1 :: Int) <$ one, Just 2 <$ two, Just 3 <$ three, Nothing <$ resetEvent ]
  |]

  mkExample "Icon Buttons" (def
    & subtitle ?~ text "Buttons groups can show groups of icons")
    [example|
  buttons (def & buttonsConfig_icon |~ True) $ do
    button def $ icon "align left" def
    button def $ icon "align center" def
    button def $ icon "align right" def
    button def $ icon "align justify" def

  buttons (def & buttonsConfig_icon |~ True) $ do
    button def $ icon "bold" def
    button def $ icon "italic" def
    button def $ icon "underline" def
  |]

  mkExample "Conditionals" (def
    & subtitle ?~ text "Buttons groups can contain conditionals")
    [example|
  buttons def $ do
    button def $ text "un"
    conditionalWithText $ pure "ou"
    button (def & buttonConfig_positive |?~ Positive) $ text "deux"
  divider def
  buttons def $ do
    button def $ text "Cancel"
    conditional
    button (def & buttonConfig_positive |?~ Positive) $ text "Save"
  |]

  mkExample "Attached group" (def
    & subtitle ?~ text "A group of buttons can be attached to the top or bottom of other content")
    [example|
  buttons (def & buttonsConfig_attached |?~ TopAttached & buttonsConfig_width |?~ Two) $ do
    button def $ text "One"
    button def $ text "Two"
  segment (def & segmentConfig_attached |?~ Attached) $ text "Segment"
  buttons (def & buttonsConfig_attached |?~ BottomAttached & buttonsConfig_width |?~ Two) $ do
    button def $ text "One"
    button def $ text "Two"
  |]

  mkExample "Horizontally Attached" (def
    & subtitle ?~ text "Buttons can be attached to the left or right of other content")
    [example|
  button (def & buttonConfig_attached |?~ Horizontally LeftAttached) $ text "Left"
  button (def & buttonConfig_attached |?~ Horizontally RightAttached) $ text "Right"
  |]

  mkExample "Vertical Buttons" (def
    & subtitle ?~ text "Groups can be formatted to appear vertically")
    [example|
  buttons (def & buttonsConfig_vertical |~ True) $ do
    button def $ text "Feed"
    button def $ text "Messages"
    button def $ text "Events"
    button def $ text "Photos"
  |]

  mkExample "Labeled Icon Buttons" (def
    & subtitle ?~ text "Groups can be formatted as labeled icons")
    [example|
  buttons (def & buttonsConfig_vertical |~ True & buttonsConfig_labeledIcon |~ True) $ do
    button def $ do
      icon "pause" def
      text "Pause"
    button def $ do
      icon "play" def
      text "Play"
    button def $ do
      icon "shuffle" def
      text "Shuffle"
  |]

  mkExample "Mixed Group" (def
    & subtitle ?~ text "Groups can be formatted to use multiple button types together")
    [example|
  buttons def $ do
    button (def & buttonConfig_labeledIcon |?~ LeftLabeled) $ do
      icon "left chevron" def
      text "Back"
    button def $ do
      icon "stop" def
      text "Stop"
    button (def & buttonConfig_labeledIcon |?~ RightLabeled) $ do
      icon "right chevron" def
      text "Forward"
  |]

  mkExample "Equal Width" (def
    & subtitle ?~ text "Groups can have their widths divided evenly")
    [example|
  buttons (def & buttonsConfig_width |?~ Five) $ do
    button def $ text "Overview"
    button def $ text "Specs"
    button def $ text "Warranty"
    button def $ text "Reviews"
    button def $ text "Support"

  divider $ def & dividerConfig_hidden |~ True

  buttons (def & buttonsConfig_width |?~ Three) $ do
    button def $ text "Overview"
    button def $ text "Specs"
    button def $ text "Support"
  |]

  mkExample "Colored Buttons" (def
    & subtitle ?~ text "Buttons can share a color")
    [example|
  buttons (def & buttonsConfig_color |?~ Blue) $ do
    button def $ text "One"
    button def $ text "Two"
    button def $ text "Three"
  |]

  mkExample "Compact Buttons" (def
    & subtitle ?~ text "Buttons can be compact")
    [example|
  buttons (def & buttonsConfig_compact |~ True) $ do
    button def $ text "One"
    button def $ text "Two"
    button def $ text "Three"
  |]

  mkExample "Floated Buttons" (def
    & subtitle ?~ text "Button groups can be floated")
    [example|
  buttons (def & buttonsConfig_floated |?~ LeftFloated) $ do
    button def $ text "One"
    button def $ text "Two"
    button def $ text "Three"

  buttons (def & buttonsConfig_floated |?~ RightFloated) $ do
    button def $ text "One"
    button def $ text "Two"
    button def $ text "Three"
  |]

  mkExample "Basic Buttons" (def
    & subtitle ?~ text "Buttons can be less pronounced")
    [example|
  buttons (def & buttonsConfig_basic |~ True) $ do
    button def $ text "One"
    button def $ text "Two"
    button def $ text "Three"

  divider $ def & dividerConfig_hidden |~ True

  buttons (def & buttonsConfig_vertical |~ True & buttonsConfig_basic |~ True) $ do
    button def $ text "One"
    button def $ text "Two"
    button def $ text "Three"
  |]

  mkExample "Group Sizes" (def
    & subtitle ?~ text "Buttons can share a size")
    [example|
  buttons (def & buttonsConfig_size |?~ Large) $ do
    button def $ text "One"
    button def $ text "Two"
    button def $ text "Three"

  divider $ def & dividerConfig_hidden |~ True

  let conf = def & buttonsConfig_size |?~ Small
                 & buttonsConfig_basic |~ True
                 & buttonsConfig_icon |~ True
  buttons conf $ do
    button def $ icon "file" def
    button def $ icon "save" def
    button def $ icon "upload" def
    button def $ icon "download" def

  divider $ def & dividerConfig_hidden |~ True

  buttons (def & buttonsConfig_size |?~ Large) $ do
    button def $ text "One"
    conditional
    button def $ text "Three"
  |]

  return ()
