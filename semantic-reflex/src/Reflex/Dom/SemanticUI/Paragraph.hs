{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.Dom.SemanticUI.Paragraph where

import Control.Lens
import Data.Default (Default (def))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Semigroup ((<>))
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Reflex
import Reflex.Dom.Core hiding
  ( checkbox, Checkbox (..), checkbox_value, checkbox_change
  , CheckboxConfig (..), checkboxConfig_attributes, checkboxConfig_setValue
  )

import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition hiding (divClass)
import Reflex.Dom.SemanticUI.Label (Label)
import Reflex.Dom.SemanticUI.Divider (Divider)
import Reflex.Dom.SemanticUI.Header (HeaderContent)

data Paragraph m a = Paragraph (Restrict Inline m a)

class PlainText (r :: k) t m where
  text :: (PostBuild t m, DomBuilder t m) => Active t Text -> Restrict r m ()
  text (Static t) = Restrict $ Reflex.Dom.Core.text t
  text (Dynamic t) = Restrict $ dynText t

instance PlainText None t m
instance PlainText Inline t m
instance PlainText HeaderContent t m
instance PlainText Divider t m
instance PlainText Label t m

data Anchor t m a = Anchor (Restrict Inline m a) (AnchorConfig t)
data AnchorConfig t = AnchorConfig
  { _href :: Active t (Maybe Text)
  , _config :: ActiveElConfig t
  }

instance Default (AnchorConfig t) where
  def = AnchorConfig
    { _href = Static Nothing
    , _config = def
    }

data AnchorResult t a = AnchorResult
  { _click :: Event t ()
  , _content :: a
  }

