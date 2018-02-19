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

import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
import Control.Lens ((<&>))
import Control.Monad (void)
import Data.Default
import Data.Foldable (for_)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Reflex
import Reflex.Dom.Core

import Reflex.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.GlobalEventHandlers as GlobalEventHandlers
import qualified GHCJS.DOM.EventM as EventM

import qualified GHCJS.DOM.Event as Event
import qualified GHCJS.DOM.Element as Element
import qualified GHCJS.DOM.HTMLCollection as HTMLCollection
import qualified GHCJS.DOM.HTMLInputElement as HTMLInputElement
import qualified GHCJS.DOM.EventTarget as EventTarget

data FormConfig t = FormConfig
  { _formElConfig :: ActiveElConfig t
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''FormConfig

instance HasElConfig t (FormConfig t) where
  elConfig = formElConfig

instance Reflex t => Default (FormConfig t) where
  def = FormConfig
    { _formElConfig = def
    }

-- | Make the form div classes from the configuration
formConfigClasses :: Reflex t => FormConfig t -> Active t Classes
formConfigClasses FormConfig {..} = dynClasses
  [ pure $ Just "ui form"
  ]

-- | Form UI Element.
form' :: MonadWidget t m => FormConfig t -> m a -> m (El t, a)
form' config@FormConfig {..} content = do
  (formEl, formResult) <- ui' "form" elConf content

  let e = DOM.uncheckedCastTo DOM.HTMLFormElement $ _element_raw formEl

  -- Reset events don't bubble to the form elements, so reset buttons cause
  -- state to be out of sync. Here we just dispatch a change event to all
  -- relevant children.
  performEvent_ $ ffor (domEvent Reset formEl) $ \ () -> do
    -- TODO: select element
    inputs <- Element.getElementsByTagName e ("input" :: Text)
    inputCount <- HTMLCollection.getLength inputs
    for_ ([0 .. inputCount]) $ \i -> HTMLCollection.item inputs i >>= \case
      Nothing -> pure ()
      Just inputEl' -> DOM.castTo DOM.HTMLInputElement inputEl' >>= \case
        Nothing -> pure ()
        Just inputEl -> do
          eventType :: Text <- HTMLInputElement.getType inputEl <&> \case
            -- Checkboxes need a "reset" event (see 'Checkbox' module)
            "checkbox" -> "reset"
            -- Text inputs need an "input" event to sync
            (_ :: Text) -> "input"
          void $ Event.newEvent eventType (Nothing :: Maybe DOM.EventInit)
            >>= EventTarget.dispatchEvent inputEl'
    textareas <- Element.getElementsByTagName e ("textarea" :: Text)
    textAreaCount <- HTMLCollection.getLength textareas
    for_ ([0 .. textAreaCount]) $ \i ->
      HTMLCollection.item textareas i >>= \case
        Nothing -> pure ()
        Just textAreaEl -> void $
          Event.newEvent ("input" :: Text) (Nothing :: Maybe DOM.EventInit)
              >>= EventTarget.dispatchEvent textAreaEl

  -- Catch the submit events to prevent the page reloading
  void $ DOM.liftJSM $ EventM.on e GlobalEventHandlers.submit $
    EventM.preventDefault

  return (formEl, formResult)
  where
    elConf = _formElConfig <> def
      { _classes = formConfigClasses config }

form :: MonadWidget t m => FormConfig t -> m a -> m a
form config = fmap snd . form' config

