{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}

{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Example.Section.Progress where

import Control.Lens
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad ((<=<))
import Data.Foldable (for_)
import Data.Semigroup ((<>))
import Reflex.Dom.SemanticUI
import Reflex.Dom.Core (text)
import System.Random (randomRIO)

import Example.QQ
import Example.Common

randomRangeControl :: MonadWidget t m => (Int, Int) -> Event t Int -> m (Event t (Int -> Int))
randomRangeControl range reset = buttons def $ do
  minus <- button (def & buttonIcon |~ True) $ icon "minus" def
  plus <- button (def & buttonIcon |~ True) $ icon "plus" def
  performEvent $ liftIO . ffor (randomRIO range) <$> leftmost
    [(+) <$ plus, subtract <$ minus, const . const <$> reset]

progressSection :: forall t m. MonadWidget t m => Section t m
progressSection = Section "Progress" blank $ do

  hscode $(printDefinition id stripParens ''ProgressConfig)

  pageHeader H3 def $ text "Examples"

  mkExample "Standard" (def
    & dynamic ?~ dynShowCode
    & subtitle ?~ text "A blank progress bar")
    [resetExample|
  \reset -> do
    update <- buttons def $ do
      minus <- button (def & buttonIcon |~ True) $ icon "minus" def
      plus <- button (def & buttonIcon |~ True) $ icon "plus" def
      pure $ leftmost [ succ <$ plus, pred <$ minus, const 4 <$ reset ]

    progress (0, 5) 4 update def
  |]

  mkExample "Standard" (def
    & subtitle ?~ text "A standard progress bar")
    [resetExample|
  \reset -> do
    update <- randomRangeControl (5,20) $ 40 <$ reset

    progress (0, 100) 40 update $ def
      & progressBar ?~ PercentageBar
      & progressLabel ?~ text "Uploading Files"
  |]

  mkExample "Indicating" (def
    & subtitle ?~ text "An indicating progress bar visually indicates the current level of progress of a task")
    $ ([str|\reset -> do
    update <- buttons def $ do
      minus <- button (def & buttonIcon |~ True) $ icon "minus" def
      plus <- button (def & buttonIcon |~ True) $ icon "plus" def
      pure $ leftmost [ succ <$ plus, pred <$ minus, const 4 <$ reset ]

    rec
      let render = dynText $ ffor dPercent $ \case
            100 -> "Project Funded!"
            x -> tshow x <> "% Funded"
      Progress _ dPercent <- progress (0, 10) 4 update $ def
        & progressLabel ?~ render
        & progressIndicating |~ True

    pure ()|]
    , Right $ \reset -> do
    update <- buttons def $ do
      minus <- button (def & buttonIcon |~ True) $ icon "minus" def
      plus <- button (def & buttonIcon |~ True) $ icon "plus" def
      pure $ leftmost [ succ <$ plus, pred <$ minus, const 4 <$ reset ]

    rec
      let render = dynText $ ffor dPercent $ \case
            100 -> "Project Funded!"
            x -> tshow x <> "% Funded"
      Progress _ dPercent <- progress (0, 10) 4 update $ def
        & progressLabel ?~ render
        & progressIndicating |~ True

    pure ()
    )

  mkExample "Activity" (def
    & subtitle ?~ text "A progress bar can show activity")
    [example|
    progress (0, 100) 40 never $ def
      & progressBar ?~ PercentageBar
      & progressActive |~ True
  |]

  mkExample "Success" (def
    & subtitle ?~ text "A progress bar can show a success state (automatic on 100%)")
    [example|
    progress (0, 100) 100 never $ def
      & progressBar ?~ PercentageBar
      & progressLabel ?~ text "Everything worked, your file is all ready."
  |]

  mkExample "Warning" (def
    & subtitle ?~ text "A progress bar can show a warning state (overrides success state)")
    [example|
    progress (0, 100) 100 never $ def
      & progressBar ?~ PercentageBar
      & progressLabel ?~ text "Your file didn't meet the minimum resolution requirements."
      & progressWarning |~ True
  |]

  mkExample "Error" (def
    & subtitle ?~ text "A progress bar can show an error state (overrides warning and success states)")
    [example|
    progress (0, 100) 100 never $ def
      & progressBar ?~ PercentageBar
      & progressLabel ?~ text "There was an error."
      & progressError |~ True
  |]

  mkExample "Disabled" (def
    & subtitle ?~ text "A progress bar can be disabled (update events are also ignored)")
    [resetExample|
  \reset -> do
    update <- randomRangeControl (5,20) $ 40 <$ reset
    disabled <- toggle True <=< button (def & buttonFloated |?~ RightFloated) $ text "Toggle Disabled"

    progress (0, 100) 40 update $ def & progressDisabled .~ disabled
  |]

  mkExample "Inverted" (def
    & subtitle ?~ text "A progress bar can have inverted colors")
    [example|
  segment (def & segmentInverted |~ True) $ do
    progress (0, 100) 6 never $ def
      & progressInverted |~ True
      & progressBar ?~ PercentageBar
      & progressLabel ?~ text "Uploading Files"
    progress (0, 100) 100 never $ def
      & progressInverted |~ True
      & progressBar ?~ PercentageBar
      & progressLabel ?~ text "Success"
    progress (0, 100) 100 never $ def
      & progressInverted |~ True
      & progressBar ?~ PercentageBar
      & progressLabel ?~ text "Warning"
      & progressWarning |~ True
    progress (0, 100) 100 never $ def
      & progressInverted |~ True
      & progressBar ?~ PercentageBar
      & progressLabel ?~ text "Error"
      & progressError |~ True
  |]

  mkExample "Attached" (def
    & subtitle ?~ text "A progress bar can be vertically attached to other elements")
    [resetExample|
  \reset -> do
    update <- randomRangeControl (5, 20) $ 50 <$ reset

    segment def $ do
      progress (0, 100) 50 update $ def & progressAttached |?~ TopAttached
      paragraph $ text "Segment with progress"
      progress (0, 100) 50 update $ def & progressAttached |?~ BottomAttached
  |]

  mkExample "Size" (def
    & subtitle ?~ text "A progress bar can vary in size")
    [resetExample|
  \reset -> do
    for_ [Tiny .. Big] $ \size ->
      progress (0, 100) 40 never $ def
        & progressSize |?~ size
        & progressLabel ?~ text (tshow size)
  |]

  mkExample "Color" (def
    & subtitle ?~ text "A progress bar can have different colors")
    [resetExample|
  \reset -> do
    for_ [minBound .. maxBound] $ \color ->
      progress (0, 100) 40 never $ def & progressColor |?~ color
  |]

  mkExample "Inverted Color" (def
    & subtitle ?~ text "A progress bar color can be inverted for improved contrast on dark backgrounds")
    [resetExample|
  \reset -> do
    segment (def & segmentInverted |~ True) $ do
      for_ [minBound .. maxBound] $ \color ->
        progress (0, 100) 40 never $ def & progressColor |?~ color & progressInverted |~ True
  |]

  mkExample "Batching" (def
    & subtitle ?~ text "Events which fire faster than the animation duration are batched")
    [resetExample|
  \reset -> do
    prog <- buttons def $ do
      go <- echo 200 0.01 <=< button (def & buttonIcon |~ True) $ icon "right arrow" def
      button (def & buttonDisabled |~ True) . display <=< holdDyn 0 $ leftmost [succ <$> go, 0 <$ reset]
      pure $ leftmost [ succ <$ go, const 0 <$ reset ]

    progress (0, 200) 0 prog $ def
      & progressLabel ?~ text "No batching"
      & progressBatchUpdates .~ False
    progress (0, 200) 0 prog $ def
      & progressLabel ?~ text "Batching: 0.1s animation"
      & progressDuration .~ 0.1
    progress (0, 200) 0 prog $ def
      & progressLabel ?~ text "Batching: 0.3s animation (default)"
    progress (0, 200) 0 prog $ def
      & progressLabel ?~ text "Batching: 0.5s animation"
      & progressDuration .~ 0.5
    progress (0, 200) 0 prog $ def
      & progressLabel ?~ text "Batching: 1s animation"
      & progressDuration .~ 1
  |]

  return ()

