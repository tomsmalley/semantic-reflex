{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

module Reflex.Dom.SemanticUI
  ( module Reflex.Dom.SemanticUI.Button
  , module Reflex.Dom.SemanticUI.Checkbox
  , module Reflex.Dom.SemanticUI.Common
  , module Reflex.Dom.SemanticUI.Dropdown
  , module Reflex.Dom.SemanticUI.Icon
  , module Reflex.Dom.SemanticUI.Image
  , module Reflex.Dom.SemanticUI.Input
  , module Reflex.Dom.SemanticUI.Label
  , module Reflex.Dom.SemanticUI.Menu
  , module Reflex.Dom.SemanticUI.Header
  , module Reflex.Dom.SemanticUI.Lenses
  , module Reflex.Dom.SemanticUI.Modal
  , module Reflex.Dom.SemanticUI.RadioGroup
  , semanticMain
  , semanticMainWithCss
  , module Reflex.Dom.Core
  ) where

import           Data.ByteString
import           Reflex.Dom.SemanticUI.Button
import           Reflex.Dom.SemanticUI.Checkbox
import           Reflex.Dom.SemanticUI.Common (tshow, Color (..), Size (..), ui, ui', ui_, Floated(..), Part (..), part, jQuery, consoleLog, consoleTime, consoleTimeEnd, catchJS, Active (..), RenderWhen(..), (|~), (|?~), HorizontalAttached (..), VerticalAttached (..), ExclusiveAttached(..), Aligned(..), zipActiveWith, DynShow(..))
import           Reflex.Dom.SemanticUI.Dropdown
import           Reflex.Dom.SemanticUI.Header
import           Reflex.Dom.SemanticUI.Icon
import           Reflex.Dom.SemanticUI.Image
import           Reflex.Dom.SemanticUI.Input
import Reflex.Dom.SemanticUI.Label
import           Reflex.Dom.SemanticUI.Menu
import           Reflex.Dom.SemanticUI.Lenses
import           Reflex.Dom.SemanticUI.Modal
import           Reflex.Dom.SemanticUI.RadioGroup
import Reflex.Dom.Core hiding
  ( checkbox, Checkbox (..), checkbox_value, checkbox_change
  , CheckboxConfig (..), checkboxConfig_attributes, checkboxConfig_setValue
  , HasSetValue (..), HasValue (..), HasAttributes (..)
  , Dropdown (..), DropdownConfig (..), dropdown_change, dropdown_value, Select
  , link, Link, Input
  )
import Language.Javascript.JSaddle (JSM)


--semanticMain :: MonadWidget t m => m () -> JSM ()
semanticMain :: (forall x. Widget x ()) -> JSM ()
semanticMain = mainWidget
--semanticMain = mainWidgetWithCss $(embedStringFile =<< makeRelativeToProject "lib/semantic.min.css")

--semanticMain :: MonadWidget t m => m () -> JSM ()
semanticMainWithCss :: ByteString -> (forall x. Widget x ()) -> JSM ()
semanticMainWithCss css = mainWidgetWithCss css
--semanticMainWithCss css = mainWidgetWithCss $ css `mappend` $(embedStringFile =<< makeRelativeToProject "lib/semantic.min.css")
