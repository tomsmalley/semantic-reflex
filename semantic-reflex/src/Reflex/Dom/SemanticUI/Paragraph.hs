module Reflex.Dom.SemanticUI.Paragraph where

import Data.Semigroup ((<>))
import Data.Text (Text)
import Reflex.Dom.Core

paragraph :: DomBuilder t m => m a -> m a
paragraph = el "p"

hyperlink :: DomBuilder t m => Text -> m a -> m a
hyperlink url = elAttr "a" attrs
  where attrs = "href" =: url

hyperlinkClass :: DomBuilder t m => Text -> Text -> m a -> m a
hyperlinkClass url c = elAttr "a" attrs
  where attrs = "href" =: url <> "class" =: c
