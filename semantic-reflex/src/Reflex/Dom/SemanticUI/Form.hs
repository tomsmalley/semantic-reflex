{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Semantic UI forms.
-- https://semantic-ui.com/collections/form.html
module Reflex.Dom.SemanticUI.Form
  (

  -- * Form
    form, form'
  , FormConfig (..)
  , formElConfig

  ) where

import Control.Lens (makeLenses)
import Control.Monad (void)
import Data.Default
import Data.Semigroup ((<>))
import Reflex
import Reflex.Dom.Core

import Reflex.Dom.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.GlobalEventHandlers as GlobalEventHandlers
import qualified GHCJS.DOM.EventM as EventM

data FormConfig t = FormConfig
  { _formElConfig :: ActiveElConfig t
  }
makeLenses ''FormConfig

instance HasElConfig t (FormConfig t) where
  elConfig = formElConfig

instance Reflex t => Default (FormConfig t) where
  def = FormConfig
    { _formElConfig = def
    }

-- | Make the form div classes from the configuration
formConfigClasses :: Reflex t => FormConfig t -> Active t Classes
formConfigClasses FormConfig {..} = activeClasses
  [ Static $ Just "ui form"
  ]

-- | Form UI Element.
form' :: MonadWidget t m => FormConfig t -> m a -> m (El t, a)
form' config@FormConfig {..} content = do
  (formEl, formResult) <- element' "form" elConf content

  let e = DOM.uncheckedCastTo DOM.HTMLFormElement $ _element_raw formEl

  -- Catch the submit events to prevent the page reloading
  void $ DOM.liftJSM $ EventM.on e GlobalEventHandlers.submit $
    EventM.preventDefault

  return (formEl, formResult)
  where
    elConf = _formElConfig <> def
      { _classes = formConfigClasses config }

form :: MonadWidget t m => FormConfig t -> m a -> m a
form config = fmap snd . form' config

