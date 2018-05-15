{-# LANGUAGE DataKinds            #-}
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

import Data.Proxy
import Control.Lens
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad ((<=<), guard)
import Data.Foldable (for_)
import Data.Maybe (fromJust)
import Data.Semigroup ((<>))
import Reflex.Dom.SemanticUI
import Reflex.Dom.Core (text)
import System.Random (randomRIO)

import Example.QQ
import Example.Common

randomRangeControl :: MonadWidget t m => (Int, Int) -> Event t Int -> m (Event t (Int -> Int))
randomRangeControl range reset = buttons def $ do
  minus <- button (def & buttonConfig_icon |~ True) $ icon "minus" def
  plus <- button (def & buttonConfig_icon |~ True) $ icon "plus" def
  performEvent $ liftIO . ffor (randomRIO range) <$> leftmost
    [(+) <$ plus, subtract <$ minus, const . const <$> reset]

data Thumbs = Up | Down deriving (Eq, Show)

thumbRating
  :: MonadWidget t m
  => Dynamic t (Int, Int) -> m (Dynamic t (Maybe Thumbs))
thumbRating state' = segment (def & segmentConfig_compact |~ True) $ mdo

  -- Don't allow either state to go below 0
  let state = state' <&> \(a, b) -> (max 0 a, max 0 b)

  let dColor a c = thumbState <&> \x -> c <$ guard (x == Just a)
  up <- button (def & buttonConfig_color .~ Dyn (dColor Up Green)) $ do
    icon "thumbs up" def
    dynText $ tshow . fst <$> dValues
  down <- button (def & buttonConfig_color .~ Dyn (dColor Down Red)) $ do
    icon "thumbs down" def
    dynText $ tshow . snd <$> dValues
  let f a (Just b) | a == b = Nothing
      f a _ = Just a
  thumbState <- foldDyn f Nothing $ leftmost [Up <$ up, Down <$ down]

  let updateState (up, down) = \case
        Nothing -> (up, down)
        Just Up -> (succ up, down)
        Just Down -> (up, succ down)
      dValues = updateState <$> state <*> thumbState
      -- fromJust is fine because we check the state is 0 or more
      range = dValues <&> \(up, down) -> Range 0 (up + down)

  progress range (fst <$> dValues) $ def
    & progressConfig_attached |?~ BottomAttached
    & progressConfig_success |?~ True
    & progressConfig_minWidth |~ False
    & progressConfig_rateLimit ?~ 0.1
    & style |~ Style "background-color: #CCC"

  pure thumbState

progressSection :: forall t m. MonadWidget t m => Section t m
progressSection = Section "Progress" blank $ do

  hscode $(printDefinition id stripParens ''ProgressConfig)

  pageHeader H3 def $ text "Examples"


  (range, upControl, downControl) <- buttons (def & buttonsConfig_icon |~ True) $ mdo
    upPlus <- button def $ icon "plus" def
    upMinus <- button def $ icon "minus" def
    let upControl = leftmost [succ <$ upPlus, pred <$ upMinus]

    range <- foldDyn ($) (Range 0 0) $ leftmost
      [ (\f (Range x y) -> Range (f x) y) <$> upControl
      , (\f (Range x y) -> Range x (f y)) <$> downControl
      ]
    button (def & buttonConfig_disabled |~ True) $ display range

    downPlus <- button def $ icon "plus" def
    downMinus <- button def $ icon "minus" def
    let downControl = leftmost [succ <$ downPlus, pred <$ downMinus]

    pure (range, upControl, downControl)

  value <- buttons (def & buttonsConfig_icon |~ True) $ do
    plus <- button def $ icon "plus" def
    reset <- button def $ icon "refresh" def
    minus <- button def $ icon "minus" def
    foldDyn ($) 0 $ leftmost [succ <$ plus, pred <$ minus, const 0 <$ reset]

  button (def & buttonConfig_disabled |~ True) $ display value

  dynShowCode =<< progress range value def

  thumbState <- foldDyn (\eF (up, down) -> case eF of
    Left f -> (f up, down)
    Right f -> (up, f down)) (0,0) $ leftmost
      [Left <$> upControl, Right <$> downControl]

  display =<< thumbRating thumbState

  mkExample "Standard" (def
    & dynamic ?~ dynShowCode
    & subtitle ?~ text "A blank progress bar")
    [resetExample|
  \reset -> do
    value <- buttons def $ do
      minus <- button (def & buttonConfig_icon |~ True) $ icon "minus" def
      plus <- button (def & buttonConfig_icon |~ True) $ icon "plus" def
      foldDyn ($) 0 $ leftmost [ succ <$ plus, pred <$ minus, const 4 <$ reset ]

    progress (pure $ Range 0 5) value def
  |]

  mkExample "Standard" (def
    & subtitle ?~ text "A standard progress bar")
    [resetExample|
  \reset -> do
    update <- randomRangeControl (5,20) $ 40 <$ reset
    value <- foldDyn ($) 40 update

    progress (pure $ Range 0 100) value $ def
      & progressConfig_bar ?~ PercentageBar
      & progressConfig_label ?~ text "Uploading Files"
  |]

  mkExample "Indicating" (def
    & subtitle ?~ text "An indicating progress bar visually indicates the current level of progress of a task")
    $ ([str|\reset -> do
    value <- buttons def $ do
      minus <- button (def & buttonConfig_icon |~ True) $ icon "minus" def
      plus <- button (def & buttonConfig_icon |~ True) $ icon "plus" def
      foldDyn ($) 4 $ leftmost [ succ <$ plus, pred <$ minus, const 4 <$ reset ]

    rec
      let render = dynText $ ffor _progressPercent $ \case
            100 -> "Project Funded!"
            x -> tshow x <> "% Funded"
      Progress {..} <- progress (pure $ Range 0 10) 4 update $ def
        & progressConfig_label ?~ render
        & progressConfig_indicating |~ True

    pure ()|]
    , Right $ \reset -> do
    value <- buttons def $ do
      minus <- button (def & buttonConfig_icon |~ True) $ icon "minus" def
      plus <- button (def & buttonConfig_icon |~ True) $ icon "plus" def
      foldDyn ($) 4 $ leftmost [ succ <$ plus, pred <$ minus, const 4 <$ reset ]

    rec
      let render = dynText $ ffor _progress_percent $ \case
            Just (Percent 100) -> "Project Funded!"
            Just p -> tshow p <> " Funded"
            Nothing -> ""
      Progress {..} <- progress (pure $ Range 0 10) value $ def
        & progressConfig_label ?~ render
        & progressConfig_indicating |~ True

    pure ()
    )

  mkExample "Activity" (def
    & subtitle ?~ text "A progress bar can show activity")
    [example|
    progress (pure $ Range 0 100) (pure 40) $ def
      & progressConfig_bar ?~ PercentageBar
      & progressConfig_active |~ True
  |]

  mkExample "Success" (def
    & subtitle ?~ text "A progress bar can show a success state (automatic on 100%)")
    [example|
    progress (pure $ Range 0 100) (pure 100) $ def
      & progressConfig_bar ?~ PercentageBar
      & progressConfig_label ?~ text "Everything worked, your file is all ready."
  |]

  mkExample "Warning" (def
    & subtitle ?~ text "A progress bar can show a warning state (overrides success state)")
    [example|
    progress (pure $ Range 0 100) (pure 100) $ def
      & progressConfig_bar ?~ PercentageBar
      & progressConfig_label ?~ text "Your file didn't meet the minimum resolution requirements."
      & progressConfig_warning |~ True
  |]

  mkExample "Error" (def
    & subtitle ?~ text "A progress bar can show an error state (overrides warning and success states)")
    [example|
    progress (pure $ Range 0 100) (pure 100) $ def
      & progressConfig_bar ?~ PercentageBar
      & progressConfig_label ?~ text "There was an error."
      & progressConfig_error |~ True
  |]

  mkExample "Disabled" (def
    & subtitle ?~ text "A progress bar can appear to be disabled")
    [resetExample|
  \reset -> do
    update <- randomRangeControl (5,20) $ 40 <$ reset
    value <- foldDyn ($) 40 update
    disabled <- toggle True <=< button (def & buttonConfig_floated |?~ RightFloated) $ text "Toggle Disabled"

    progress (pure $ Range 0 100) value $ def & progressConfig_disabled .~ disabled
  |]

  mkExample "Inverted" (def
    & subtitle ?~ text "A progress bar can have inverted colors")
    [example|
  segment (def & segmentConfig_inverted |~ True) $ do
    let range = Range 0 100
    progress (pure range) (pure 6) $ def
      & progressConfig_inverted |~ True
      & progressConfig_bar ?~ PercentageBar
      & progressConfig_label ?~ text "Uploading Files"
    progress (pure range) (pure 100) $ def
      & progressConfig_inverted |~ True
      & progressConfig_bar ?~ PercentageBar
      & progressConfig_label ?~ text "Success"
    progress (pure range) (pure 100) $ def
      & progressConfig_inverted |~ True
      & progressConfig_bar ?~ PercentageBar
      & progressConfig_label ?~ text "Warning"
      & progressConfig_warning |~ True
    progress (pure range) (pure 100) $ def
      & progressConfig_inverted |~ True
      & progressConfig_bar ?~ PercentageBar
      & progressConfig_label ?~ text "Error"
      & progressConfig_error |~ True
  |]

  mkExample "Attached" (def
    & subtitle ?~ text "A progress bar can be vertically attached to other elements")
    [resetExample|
  \reset -> do
    value <- foldDyn ($) 50 <=< randomRangeControl (5, 20) $ 50 <$ reset

    let range = Range 0 100
    segment def $ do
      progress (pure range) value $ def & progressConfig_attached |?~ TopAttached
      paragraph $ text "Segment with progress"
      progress (pure range) value $ def & progressConfig_attached |?~ BottomAttached
  |]

  mkExample "Size" (def
    & subtitle ?~ text "A progress bar can vary in size")
    [resetExample|
  \reset -> do
    for_ [Tiny .. Big] $ \size ->
      progress (pure $ Range 0 100) (pure 40) $ def
        & progressConfig_size |?~ size
        & progressConfig_label ?~ text (tshow size)
  |]

  mkExample "Color" (def
    & subtitle ?~ text "A progress bar can have different colors")
    [resetExample|
  \reset -> do
    for_ [minBound .. maxBound] $ \color ->
      progress (pure $ Range 0 100) (pure 40) $ def & progressConfig_color |?~ color
  |]

  mkExample "Inverted Color" (def
    & subtitle ?~ text "A progress bar color can be inverted for improved contrast on dark backgrounds")
    [resetExample|
  \reset -> do
    segment (def & segmentConfig_inverted |~ True) $ do
      for_ [minBound .. maxBound] $ \color ->
        progress (pure $ Range 0 100) (pure 40) $ def & progressConfig_color |?~ color & progressConfig_inverted |~ True
  |]

  mkExample "Batching" (def
    & subtitle ?~ text "Events which fire faster than the animation duration are batched")
    [resetExample|
  \reset -> do
    prog <- buttons def $ do
      go <- echo_ 200 0.01 <=< button (def & buttonConfig_icon |~ True) $ icon "right arrow" def
      button (def & buttonConfig_disabled |~ True) . display <=< holdDyn 0 $ leftmost [succ <$> go, 0 <$ reset]
      foldDyn ($) 0 $ leftmost [ succ <$ go, const 0 <$ reset ]

    let range = Range 0 200
    progress (pure range) prog $ def
      & progressConfig_label ?~ text "No batching"
      & progressConfig_rateLimit .~ Nothing
    progress (pure range) prog $ def
      & progressConfig_label ?~ text "Batching: 0.1s animation"
      & progressConfig_rateLimit ?~ 0.1
    progress (pure range) prog $ def
      & progressConfig_label ?~ text "Batching: 0.3s animation (default)"
    progress (pure range) prog $ def
      & progressConfig_label ?~ text "Batching: 0.5s animation"
      & progressConfig_rateLimit ?~ 0.5
    progress (pure range) prog $ def
      & progressConfig_label ?~ text "Batching: 1s animation"
      & progressConfig_rateLimit ?~ 1
  |]

  return ()

