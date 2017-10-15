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

import Data.Default
import Data.Text (Text)
import Reflex
import Reflex.Dom.Core

import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition hiding (divClass)

import Reflex.Dom.SemanticUI.Button (Button)
import Reflex.Dom.SemanticUI.Label (Label)
import Reflex.Dom.SemanticUI.Divider (ContentDivider)
import Reflex.Dom.SemanticUI.Header (Header)
import Reflex.Dom.SemanticUI.Message (Message)

data Paragraph m a = Paragraph (Restrict Inline m a)

class PlainText (r :: k) t m where
  text :: (PostBuild t m, DomBuilder t m) => Active t Text -> Restrict r m ()
  text (Static t) = Restrict $ Reflex.Dom.Core.text t
  text (Dynamic t) = Restrict $ dynText t

instance PlainText None t m
instance PlainText Inline t m
instance PlainText Header t m
instance PlainText ContentDivider t m
instance PlainText Label t m
instance PlainText Button t m

class HasParagraph (r :: k) t m where
  paragraph :: MonadWidget t m => Restrict Inline m a -> Restrict r m a
  paragraph m = reRestrict $ elWithAnim "p" def m

instance HasParagraph None t m
instance HasParagraph Message t m

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

