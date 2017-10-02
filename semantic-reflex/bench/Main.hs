{-# LANGUAGE OverloadedStrings #-}

-- | Testing speed differences between dynamic and static dom generation
module Main where

import Control.Monad (replicateM_, (<=<), void)
import Control.Monad.Fix (MonadFix)
import Data.Text (Text)
import Reflex
import Reflex.Dom.Core

data Active t a
  = Static !a
  | Dynamic !(Dynamic t a)

putActive :: (PostBuild t m, DomBuilder t m) => Active t Text -> m ()
putActive (Static a) = putStatic a
putActive (Dynamic a) = putDynamic a

putStatic :: DomBuilder t m => Text -> m ()
putStatic = el "p" . text

putDynamic :: (PostBuild t m, DomBuilder t m) => Dynamic t Text -> m ()
putDynamic = el "p" . dynText

-- | Given an event and a list of widgets, switch between the widgets on each
-- occurance of the event. Does not cycle the list.
ratchet :: (MonadHold t m, MonadFix m, DomBuilder t m) => Event t b -> [m a] -> m ()
ratchet _ [] = blank
ratchet evt (w:ws) = void $ widgetHold w =<< zipListWithEvent const ws evt

-- | Alternate between the elements of the list and the given element.
-- Like intersperse, but adds an extra element at the end.
alternate :: a -> [a] -> [a]
alternate _ [] = []
alternate y (x : xs) = x : y : alternate y xs

main :: IO ()
main = mainWidget $ do

  switchEvent <- domEvent Click . fst <$> el' "button" (text "Switch")

  flipEvent <- toggle False . domEvent Click . fst <=< el' "button" $ text "Flip dyns"

  let dynContent = (\f -> if f then "dynamic.." else "DYNAMIC..") <$> flipEvent

      static = do
        el "h1" $ text "Static"
        replicateM_ 3000 $ putStatic "static..."
      pureDynamic = do
        el "h1" $ text "Pure Dynamic"
        replicateM_ 3000 $ putDynamic $ pure "dynamic.."
      dynamic = do
        el "h1" $ text "Dynamic"
        replicateM_ 3000 $ putDynamic dynContent
      activeStatic = do
        el "h1" $ text "Active Static"
        replicateM_ 3000 $ putActive $ Static "static..."
      activePureDynamic = do
        el "h1" $ text "Active Pure Dynamic"
        replicateM_ 3000 $ putActive $ Dynamic $ pure "dynamic.."
      activeDynamic = do
        el "h1" $ text "Active Dynamic"
        replicateM_ 3000 $ putActive $ Dynamic dynContent

      examples = [static, activeStatic, pureDynamic, activePureDynamic, dynamic, activeDynamic]

  ratchet switchEvent $ cycle $ alternate (el "h1" $ text "cleared") examples
