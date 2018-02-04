module Reflex.Dom.SemanticUI.Paragraph where

import Data.Semigroup ((<>))
import Data.Text (Text)
import Reflex.Dom.Core

paragraph :: MonadWidget t m => m a -> m a
paragraph = el "p"

hyperlink :: MonadWidget t m => Text -> m a -> m a
hyperlink url = elAttr "a" attrs
  where attrs = "href" =: url

hyperlinkClass :: MonadWidget t m => Text -> Text -> m a -> m a
hyperlinkClass url c = elAttr "a" attrs
  where attrs = "href" =: url <> "class" =: c
