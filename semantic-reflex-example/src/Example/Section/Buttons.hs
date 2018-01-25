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
buttonSection = LinkedSection "Button" (text "") $ do

  paragraph $ text ""

  --hscode $(printDefinition id stripParens ''Button)
  hscode $(printDefinition id stripParens ''ButtonConfig)

  pageHeader H3 def $ text "Types"

  mkExample "Button" (def & subtitle ?~ text "A standard button")
    [example|
  button def $ text "Button"
  button (def & buttonType .~ DivButton) $ text "Div Button"
  button (def & buttonType .~ LinkButton) $ text "Link Button"
  |]

  mkExample "Emphasis" (def
    & subtitle ?~ text "A button can have a different level of emphasis")
    [example|
  button (def & buttonEmphasis |?~ Primary) $ text "Primary"
  button (def & buttonEmphasis |?~ Secondary) $ text "Secondary"
  -- Tertiary has no special styles for buttons
  button (def & buttonEmphasis |?~ Tertiary) $ text "Tertiary"
  button def $ text "Normal"
  |]

  mkExample "Animated" (def
    & subtitle ?~ text "A button animate to show hidden content")
    [example|
  let rightAnim = def & animatedButtonHiddenContent .~ icon "right arrow" def
  button (def & buttonAnimated ?~ rightAnim) $ text "Next"

  let shopAnim = def
        & animatedButtonHiddenContent .~ text "Shop"
        & animatedButtonType |~ VerticalAnimated
  button (def & buttonAnimated ?~ shopAnim) $ icon "shop" def

  let priceAnim = def
        & animatedButtonHiddenContent .~ text "£12.99 a month"
        & animatedButtonType |~ AnimatedFade
  button (def & buttonAnimated ?~ priceAnim) $ text "Sign-up for a Pro account"
  |]

  mkExample "Labeled" (def
    & subtitle ?~ text "A button can have a label")
    [example|
  labeledButton def $ do
    button (def & buttonColor |?~ Red) $ do
      icon "heart" def
      text "Like"
    let conf = def
          & labelBasic |~ True
          & labelColor |?~ Red
          & labelPointing |?~ LeftPointing
    label conf $ text "256"

  labeledButton (def & labeledButtonSide |~ LeftLabeled) $ do
    let conf = def
          & labelBasic |~ True
          & labelColor |?~ Blue
          & labelPointing |?~ RightPointing
    label conf $ text "38"
    button (def & buttonColor |?~ Blue & buttonBasic |~ True) $ do
      icon "fork" def
      text "Fork"

  labeledButton (def & labeledButtonSide |~ LeftLabeled) $ do
    label (def & labelBasic |~ True) $ text "1,048"
    button (def & buttonIcon |~ True) $ do
      icon "fork" def
  |]

  mkExample "Icon" (def
    & subtitle ?~ text "A button can have only an icon")
    [example|
  button (def & buttonIcon |~ True) $ do
    icon "cloud" def
  |]

  mkExample "Labeled Icon" (def
    & subtitle ?~ text "A button can use an icon as a label")
    [example|
  button (def & buttonLabeledIcon |?~ LeftLabeled) $ do
    icon "pause" def
    text "Pause"
  button (def & buttonLabeledIcon |?~ RightLabeled) $ do
    icon "play" def
    text "Play"
  |]

  mkExample "Basic" (def
    & subtitle ?~ text "A basic button is less pronounced")
    [example|
  button (def & buttonBasic |~ True) $ do
    icon "user" def
    text "Add Friend"
  button (def & buttonBasic |~ True & buttonEmphasis |?~ Primary) $
    text "Primary"
  button (def & buttonBasic |~ True & buttonEmphasis |?~ Secondary) $
    text "Secondary"
  button (def & buttonBasic |~ True & buttonPositive |?~ Positive) $
    text "Positive"
  button (def & buttonBasic |~ True & buttonPositive |?~ Negative) $
    text "Negative"
  |]

  mkExample "Inverted" (def
    & subtitle ?~ text "A button can be formatted to appear on dark backgrounds")
    [example|
  segment (def & segmentInverted |~ True) $ do
    for_ [minBound .. maxBound] $ \c -> do
      let conf = def & buttonInverted |~ True
                     & buttonColor |?~ c
                     & style |~ Style ("margin-bottom" =: "0.75em")
      button conf $ text $ tshow c
    divider def
    for_ [minBound .. maxBound] $ \c -> do
      let conf = def & buttonBasic |~ True
                     & buttonInverted |~ True
                     & buttonColor |?~ c
                     & style |~ Style ("margin-bottom" =: "0.75em")
      button conf $ text $ "Basic " <> tshow c
  |]

  mkExample "Disabled" (def
    & subtitle ?~ text "A button can show it is currently unable to be interacted with")
    [example|
  button (def & buttonDisabled |~ True) $ do
    icon "user" def
    text "Followed"
  |]

  mkExample "Loading" (def
    & subtitle ?~ text "A button can show a loading indicator")
    [example|
  start <- button def $ text "Start"
  stop <- button def $ text "Stop"
  isLoading <- holdDyn True $ leftmost [True <$ start, False <$ stop]
  divider (def & dividerHidden |~ True)
  button (def & buttonLoading .~ Dynamic isLoading) $ text "Loaded"
  button (def & buttonBasic |~ True
              & buttonLoading .~ Dynamic isLoading) $ text "Loaded"
  button (def & buttonEmphasis |?~ Primary
              & buttonLoading .~ Dynamic isLoading) $ text "Loaded"
  button (def & buttonEmphasis |?~ Secondary
              & buttonLoading .~ Dynamic isLoading) $ text "Loaded"
  |]

  mkExample "Social" (def
    & subtitle ?~ text "A button can be formatted in the style of a social website")
    [example|
  for_ [minBound .. maxBound] $ \s -> do
    let conf = def & buttonSocial |?~ s
                   & style |~ Style ("margin-bottom" =: "0.75em")
    button conf $ do
     icon (pure $ toClassText s) def
     text $ tshow s
  |]

  mkExample "Size" (def
    & subtitle ?~ text "A button can have different sizes")
    [example|
  for_ [minBound .. maxBound] $ \s -> do
    button (def & buttonSize |?~ s) $ text $ tshow s
  |]

  mkExample "Floated" (def
    & subtitle ?~ text "A button can be floated")
    [example|
  button (def & buttonFloated |?~ LeftFloated) $ text "Left Floated"
  button (def & buttonFloated |?~ RightFloated) $ text "Right Floated"
  |]

  mkExample "Color" (def
    & subtitle ?~ text "A button can have different colors")
    [example|
  for_ [minBound .. maxBound] $ \c -> do
    let conf = def & buttonColor |?~ c
                   & style |~ Style ("margin-bottom" =: "0.75em")
    button conf $ text $ tshow c
  |]

  mkExample "Compact" (def
    & subtitle ?~ text "A button can reduce its padding")
    [example|
  button (def & buttonCompact |~ True) $ text "Hold"
  button (def & buttonCompact |~ True
              & buttonIcon |~ True) $ icon "pause" def
  button (def & buttonCompact |~ True
              & buttonLabeledIcon |?~ LeftLabeled) $ do
    icon "pause" def
    text "Pause"
  |]

  mkExample "Toggle" (def
    & subtitle ?~ text "A button can be toggled"
    & dynamic ?~ dynCode
    & inbetween ?~ (message (def & messageType |?~ InfoMessage)
      $ paragraph $ do
        icon "info circle" def
        text "Semantic UI has some sneaky javascript that gives state to anything with a 'toggle' class. Here you must deal with state explicitly."))
    ([str|
  \resetEvent -> do
    rec let conf = def & emphasis .~ Dynamic (bool (Just Primary) Nothing <$> on)
        on <- toggle False <=< button conf $ text "Toggle"
    return on
  |], Left $ do
    rec let conf = def & buttonEmphasis .~ Dynamic
              (bool Nothing (Just Primary) <$> on)
        on <- toggle False <=< button conf $ text "Toggle"
    return on
    )

  mkExample "Fluid" (def
    & subtitle ?~ text "A button can fill its container")
    [example|
  button (def & buttonFluid |~ True) $ text "Fits container"
  |]

  mkExample "Circular" (def
    & subtitle ?~ text "A button can be circular")
    [example|
  button (def & buttonCircular |~ True & buttonIcon |~ True) $
    icon "settings" def
  for_ [minBound .. maxBound] $ \s -> do
    let conf = def & buttonSocial |?~ s
                   & buttonCircular |~ True
                  & buttonIcon |~ True
    button conf $ icon (pure $ toClassText s) def
  |]

  mkExample "Attached" (def
    & subtitle ?~ text "A button can be attached to the top or bottom of other content"
    & inbetween ?~ upstreamIssue 5851 "Attached 'Button' elements are incorrectly sized, use an alternate tag until this is fixed upstream.")
    [example|
  let conf = def & buttonType .~ DivButton
  button (conf & buttonAttached |?~ Vertically TopAttached) $ text "Top"
  segment (def & segmentAttached |?~ Attached) $ text "Segment"
  button (conf & buttonAttached |?~ Vertically BottomAttached) $ text "Bottom"
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
  buttons (def & buttonsIcon |~ True) $ do
    button def $ icon "align left" def
    button def $ icon "align center" def
    button def $ icon "align right" def
    button def $ icon "align justify" def

  buttons (def & buttonsIcon |~ True) $ do
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
    button (def & buttonPositive |?~ Positive) $ text "deux"
  divider def
  buttons def $ do
    button def $ text "Cancel"
    conditional
    button (def & buttonPositive |?~ Positive) $ text "Save"
  |]

  mkExample "Attached group" (def
    & subtitle ?~ text "A group of buttons can be attached to the top or bottom of other content")
    [example|
  buttons (def & buttonsAttached |?~ TopAttached & buttonsWidth |?~ Two) $ do
    button def $ text "One"
    button def $ text "Two"
  segment (def & segmentAttached |?~ Attached) $ text "Segment"
  buttons (def & buttonsAttached |?~ BottomAttached & buttonsWidth |?~ Two) $ do
    button def $ text "One"
    button def $ text "Two"
  |]

  mkExample "Horizontally Attached" (def
    & subtitle ?~ text "Buttons can be attached to the left or right of other content")
    [example|
  button (def & buttonAttached |?~ Horizontally LeftAttached) $ text "Left"
  button (def & buttonAttached |?~ Horizontally RightAttached) $ text "Right"
  |]

  mkExample "Vertical Buttons" (def
    & subtitle ?~ text "Groups can be formatted to appear vertically")
    [example|
  buttons (def & buttonsVertical |~ True) $ do
    button def $ text "Feed"
    button def $ text "Messages"
    button def $ text "Events"
    button def $ text "Photos"
  |]

  mkExample "Labeled Icon Buttons" (def
    & subtitle ?~ text "Groups can be formatted as labeled icons")
    [example|
  buttons (def & buttonsVertical |~ True & buttonsLabeledIcon |~ True) $ do
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
    button (def & buttonLabeledIcon |?~ LeftLabeled) $ do
      icon "left chevron" def
      text "Back"
    button def $ do
      icon "stop" def
      text "Stop"
    button (def & buttonLabeledIcon |?~ RightLabeled) $ do
      icon "right chevron" def
      text "Forward"
  |]

  mkExample "Equal Width" (def
    & subtitle ?~ text "Groups can have their widths divided evenly")
    [example|
  buttons (def & buttonsWidth |?~ Five) $ do
    button def $ text "Overview"
    button def $ text "Specs"
    button def $ text "Warranty"
    button def $ text "Reviews"
    button def $ text "Support"

  divider $ def & dividerHidden |~ True

  buttons (def & buttonsWidth |?~ Three) $ do
    button def $ text "Overview"
    button def $ text "Specs"
    button def $ text "Support"
  |]

  mkExample "Colored Buttons" (def
    & subtitle ?~ text "Buttons can share a color")
    [example|
  buttons (def & buttonsColor |?~ Blue) $ do
    button def $ text "One"
    button def $ text "Two"
    button def $ text "Three"
  |]

  mkExample "Compact Buttons" (def
    & subtitle ?~ text "Buttons can be compact")
    [example|
  buttons (def & buttonsCompact |~ True) $ do
    button def $ text "One"
    button def $ text "Two"
    button def $ text "Three"
  |]

  mkExample "Floated Buttons" (def
    & subtitle ?~ text "Button groups can be floated")
    [example|
  buttons (def & buttonsFloated |?~ LeftFloated) $ do
    button def $ text "One"
    button def $ text "Two"
    button def $ text "Three"

  buttons (def & buttonsFloated |?~ RightFloated) $ do
    button def $ text "One"
    button def $ text "Two"
    button def $ text "Three"
  |]

  mkExample "Basic Buttons" (def
    & subtitle ?~ text "Buttons can be less pronounced")
    [example|
  buttons (def & buttonsBasic |~ True) $ do
    button def $ text "One"
    button def $ text "Two"
    button def $ text "Three"

  divider $ def & dividerHidden |~ True

  buttons (def & buttonsVertical |~ True & buttonsBasic |~ True) $ do
    button def $ text "One"
    button def $ text "Two"
    button def $ text "Three"
  |]

  mkExample "Group Sizes" (def
    & subtitle ?~ text "Buttons can share a size")
    [example|
  buttons (def & buttonsSize |?~ Large) $ do
    button def $ text "One"
    button def $ text "Two"
    button def $ text "Three"

  divider $ def & dividerHidden |~ True

  let conf = def & buttonsSize |?~ Small
                 & buttonsBasic |~ True
                 & buttonsIcon |~ True
  buttons conf $ do
    button def $ icon "file" def
    button def $ icon "save" def
    button def $ icon "upload" def
    button def $ icon "download" def

  divider $ def & dividerHidden |~ True

  buttons (def & buttonsSize |?~ Large) $ do
    button def $ text "One"
    conditional
    button def $ text "Three"
  |]

  return ()

