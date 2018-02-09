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
import Control.Monad ((<=<))
import Reflex.Dom.SemanticUI
import Reflex.Dom.Core (text)

import Example.QQ
import Example.Common

progressSection :: forall t m. MonadWidget t m => Section t m
progressSection = Section "Progress" blank $ do

  hscode $(printDefinition id stripParens ''ProgressConfig)

  pageHeader H3 def $ text "Examples"

  mkExample "Standard" (def
    & dynamic ?~ dynShowCode
    & subtitle ?~ text "A basic blank progress bar")
    [resetExample|
  \reset -> do
    prog <- buttons def $ do
      plus <- button (def & buttonIcon |~ True) $ icon "plus" def
      minus <- button (def & buttonIcon |~ True) $ icon "minus" def
      pure $ leftmost [ succ <$ plus, pred <$ minus, const 4 <$ reset ]

    progress (0, 5) 4 prog def
  |]

  eUpdate <- buttons def $ do
    plus <- button (def & buttonIcon |~ True) $ icon "plus" def
    reset <- button (def & buttonIcon |~ True) $ icon "refresh" def
    minus <- button (def & buttonIcon |~ True) $ icon "minus" def
    pure $ leftmost [ succ <$ plus, pred <$ minus, const 4 <$ reset ]

  rec Progress value _ <- progress (0, 10) 4 eUpdate $ def
        & progressBar ?~ PercentageBar
        & progressIndicating |~ True
        & progressActive |~ True
        & progressLabel ?~ display value

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

