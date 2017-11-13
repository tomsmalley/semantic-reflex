{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Rank2Types #-}

module Reflex.Dom.SemanticUI
  ( module Reflex.Dom.Active
  , module Reflex.Dom.SemanticUI.Class
  , module Reflex.Dom.SemanticUI.Common
  , module Reflex.Dom.SemanticUI.Lenses
  , module Components

  , module Reflex.Dom.Core
  ) where

import Data.ByteString
import Reflex.Dom.Active
import Reflex.Dom.SemanticUI.Class
import Reflex.Dom.SemanticUI.Common -- (tshow, Color (..), Size (..), Floated(..), jQuery, consoleLog, consoleTime, consoleTimeEnd, catchJS, (|~), (|?~), HorizontalAttached (..), VerticalAttached (..), ExclusiveAttached(..), Aligned(..), zipActiveWith, DynShow(..), Classes(..), Style(..), Width(..), mapComponent, Component(..), None, activeText, Inline, staticText, Emphasis(..), countWithLast, Positive(..), Social(..), ToClassText(..), unComponent, reComponent, addClass, runComponent, Labeled(..), keyIs, HasHeader(..))
import Reflex.Dom.SemanticUI.Lenses

import Reflex.Dom.SemanticUI.Button     as Components
import Reflex.Dom.SemanticUI.Checkbox   as Components
import Reflex.Dom.SemanticUI.Container  as Components
import Reflex.Dom.SemanticUI.Dimmer     as Components
import Reflex.Dom.SemanticUI.Divider    as Components
import Reflex.Dom.SemanticUI.Dropdown   as Components
import Reflex.Dom.SemanticUI.Field      as Components
import Reflex.Dom.SemanticUI.Form       as Components
import Reflex.Dom.SemanticUI.Header     as Components
import Reflex.Dom.SemanticUI.Icon       as Components
import Reflex.Dom.SemanticUI.Image      as Components
import Reflex.Dom.SemanticUI.Input      as Components
import Reflex.Dom.SemanticUI.Label      as Components
import Reflex.Dom.SemanticUI.List       as Components
import Reflex.Dom.SemanticUI.Menu       as Components
import Reflex.Dom.SemanticUI.Message    as Components
import Reflex.Dom.SemanticUI.Paragraph  as Components
import Reflex.Dom.SemanticUI.Segment    as Components
import Reflex.Dom.SemanticUI.Sticky     as Components
import Reflex.Dom.SemanticUI.Transition as Components

import Reflex.Dom.Core hiding
  ( checkbox, Checkbox (..), checkbox_value, checkbox_change
  , CheckboxConfig (..), checkboxConfig_attributes, checkboxConfig_setValue
  , HasSetValue (..), HasValue (..), HasAttributes (..)
  , Dropdown (..), DropdownConfig (..), dropdown_change, dropdown_value, Select
  , link, Link, Input, Drop
  , text, tag, Error
  , element, element'
  , textInput, TextInput(..), TextInputConfig(..), keypress
  )
import Language.Javascript.JSaddle (JSM)

