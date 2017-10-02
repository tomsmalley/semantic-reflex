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
labels = LinkedSection "Label" "" $ do

  $(printDefinition stripParens ''Label)
  $(printDefinition stripParens ''LabelConfig)

  ui $ Header H3 (text "Types") def

  divClass "ui equal width stackable grid" $ do

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Label" "" [mkExample|
        ui $ Label "Veronika" $ def
          & image .~ AlwaysRender (Image "https://semantic-ui.com/images/avatar/small/veronika.jpg" def)
          & detail |?~ "Friend"
          & color |?~ Blue
        ui $ Label "Jenny" $ def
          & image .~ AlwaysRender (Image "https://semantic-ui.com/images/avatar/small/jenny.jpg" def)
          & detail |?~ "Student"
          & color |?~ Teal
        ui $ Label "Christian" $ def
          & image .~ AlwaysRender (Image "https://semantic-ui.com/images/avatar/small/christian.jpg" def)
          & detail |?~ "Co-worker"
          & color |?~ Yellow
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCardReset "Label" "" [mkExample|
        \resetEvent -> do
          let src person = "https://semantic-ui.com/images/avatar/small/" <> person <> ".jpg"
              mkLabel (person, url, colour) = do
                LabelResult _ iconResult _ <- ui $ Label (pure person) $ def
                  & image .~ AlwaysRender (Image (pure $ src url) def)
                  & rightIcon .~ AlwaysRender (Icon "delete" def)
                  & color |?~ colour
                return $ switch . current $ fmap (maybe never (domEvent Click . fst)) iconResult
          mapM_ (removableWidget resetEvent . mkLabel)
            [ ("Justen", "justen", Grey)
            , ("Adrienne", "ade", Blue)
            , ("Zoe", "zoe", Teal)
            , ("Elliot", "elliot", Olive)
            , ("Joe", "joe", Yellow)
            , ("Matt", "matt", Orange)
            ]
        |]

  return ()

