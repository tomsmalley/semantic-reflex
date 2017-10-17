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

import Control.Monad.Fix
import Data.Default
import Data.Text (Text)
import Reflex
import Reflex.Dom.Core

import Reflex.Dom.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition hiding (divClass)

import Reflex.Dom.SemanticUI.Button (Button)
import Reflex.Dom.SemanticUI.Label (Label)
import Reflex.Dom.SemanticUI.Divider (ContentDivider)
import Reflex.Dom.SemanticUI.Header (Header)
import Reflex.Dom.SemanticUI.Message (Message)

class PlainText (r :: k) t m where
  text :: (MonadHold t m, MonadFix m, PostBuild t m, DomBuilder t m) => Active t Text -> Component r m ()
  text (Static t) = Component $ Reflex.Dom.Core.text t
  text (Dynamic t) = Component $ dynText t

instance PlainText None t m
instance PlainText Inline t m
instance PlainText Header t m
instance PlainText ContentDivider t m
instance PlainText Label t m
instance PlainText Button t m

class HasParagraph (r :: k) t m where
  paragraph :: MonadWidget t m => Component Inline m a -> Component r m a
  paragraph m = reComponent $ elWithAnim "p" def m

instance HasParagraph None t m
instance HasParagraph Message t m

data Anchor t m a = Anchor (Component Inline m a) (AnchorConfig t)
data AnchorConfig t = AnchorConfig
  { _href :: Active t (Maybe Text)
  , _config :: ActiveElConfig t
  }

instance Reflex t => Default (AnchorConfig t) where
  def = AnchorConfig
    { _href = pure Nothing
    , _config = def
    }

data AnchorResult t a = AnchorResult
  { _click :: Event t ()
  , _content :: a
  }

