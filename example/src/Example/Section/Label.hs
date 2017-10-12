{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}

module Example.Section.Label where

import Control.Lens
import Control.Monad ((<=<), void, when, join)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.SemanticUI

import Example.QQ
import Example.Common

labels :: forall t m. MonadWidget t m => Section m
labels = LinkedSection "Label" blank $ do

  hscode $ $(printDefinition id stripParens ''Label)
  hscode $ $(printDefinition id stripParens ''LabelConfig)

  ui $ Header H3 (ui $ Text "Types") def

  divClass "ui equal width stackable grid" $ do

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Label" "" [mkExample|
        ui $ Label "Veronika" $ def
          & image .~ AlwaysRender (Image "https://semantic-ui.com/images/avatar/small/veronika.jpg" def)
          & detail .~ AlwaysRender "Friend"
          & color |?~ Blue
        ui $ Label "Jenny" $ def
          & image .~ AlwaysRender (Image "https://semantic-ui.com/images/avatar/small/jenny.jpg" def)
          & detail .~ AlwaysRender "Student"
          & color |?~ Teal
        ui $ Label "Christian" $ def
          & image .~ AlwaysRender (Image "https://semantic-ui.com/images/avatar/small/christian.jpg" def)
          & detail .~ AlwaysRender "Co-worker"
          & color |?~ Yellow
        |]

    resetEvent <- ui $ Button "Reset" def
    divClass "row" $ do
      divClass "column" $ do
--        exampleCardReset "Label" "" [mkExample|
--        \resetEvent -> do
          let src person = "https://semantic-ui.com/images/avatar/small/" <> person <> ".jpg"
              mkLabel (person, url, colour) = do
                rec
                  let onClose = switch . current $ fmap (maybe never (domEvent Click . fst)) iconResult
                  LabelResult _ iconResult _ <- ui $ Label (pure person) $ def
                    & image .~ AlwaysRender (Image (pure $ src url) def)
                    & rightIcon .~ AlwaysRender (Icon "delete" def)
                    & color |?~ colour
                    & transition ?~ leftmost
                      [ Transition Instant (def & direction ?~ In) <$ resetEvent
                      , Transition Scale (def & direction ?~ Out) <$ onClose ]
                return ()

          mapM_ mkLabel
            [ ("Justen", "justen", Grey)
            , ("Adrienne", "ade", Blue)
            , ("Zoe", "zoe", Teal)
            , ("Elliot", "elliot", Olive)
            , ("Joe", "joe", Yellow)
            , ("Matt", "matt", Orange)
            ]
--        |]

  return ()

