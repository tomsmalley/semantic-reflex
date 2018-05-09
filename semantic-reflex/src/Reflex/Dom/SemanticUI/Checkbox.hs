{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}

-- | Semantic UI checkboxes. Pure reflex implementation is provided.
-- https://semantic-ui.com/modules/checkbox.html
-- TODO:
-- - Read-only and uncheckable states
-- - Blur on escape key
-- - Toggle on enter key
module Reflex.Dom.SemanticUI.Checkbox where

#ifdef USE_TEMPLATE_HASKELL
import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
#else
import Control.Lens.Type
#endif

import Control.Lens hiding (element)
import Control.Monad ((<=<), void, guard)
import Control.Monad.Trans.Maybe
import Data.Default
import Data.Foldable (for_)
import Data.Functor.Misc (WrapArg(..))
import Data.Proxy
import Data.Semigroup ((<>))
import Data.Traversable (for)
import GHCJS.DOM (currentDocument)
import GHCJS.DOM.DocumentOrShadowRoot (getActiveElement)
import GHCJS.DOM.Types (castTo, HTMLElement(..), Element(..))
import Reflex
import Reflex.Dom.Core hiding (CheckboxConfig, Checkbox(..), SetValue(..))

import Reflex.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

import qualified GHCJS.DOM.HTMLElement as HTMLElement
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.HTMLInputElement as Input
import Language.Javascript.JSaddle (liftJSM)

-- | Checkbox types. If you need a radio type, see "Reflex.Dom.SemanticUI.RadioGroup".
data CheckboxType =  Slider | Toggle deriving (Eq, Show)

instance ToClassText CheckboxType where
  toClassText Slider = "slider"
  toClassText Toggle = "toggle"

-- | Configuration of a checkbox. Value and indeterminate are split into initial
-- and set events in order to logically disconnect them from their dynamic
-- return values in Checkbox.
data CheckboxConfig t = CheckboxConfig
  { _checkboxConfig_setValue :: SetValue t Bool
  -- ^ Value control

  , _checkboxConfig_setIndeterminate :: SetValue t Bool
  -- ^ Control over indeterminate state

  , _checkboxConfig_type :: Active t (Maybe CheckboxType)
  -- ^ Checkbox type, e.g. slider
  , _checkboxConfig_fitted :: Active t Bool
  -- ^ Checkbox is fitted
  , _checkboxConfig_disabled :: Active t Bool
  -- ^ Checkbox is disabled

  , _checkboxConfig_elConfig :: ActiveElConfig t
  -- ^ Config
  }
#ifdef USE_TEMPLATE_HASKELL
makeLensesWith (lensRules & simpleLenses .~ True) ''CheckboxConfig
#endif

instance HasElConfig t (CheckboxConfig t) where
  elConfig = checkboxConfig_elConfig

instance Reflex t
  => Default (CheckboxConfig t) where
  def = CheckboxConfig
    { _checkboxConfig_setValue = SetValue False Nothing
    , _checkboxConfig_setIndeterminate = SetValue False Nothing

    , _checkboxConfig_type = pure Nothing
    , _checkboxConfig_fitted = pure False
    , _checkboxConfig_disabled = pure False
    , _checkboxConfig_elConfig = def
    }

-- | Make the checkbox div classes from the configuration
checkboxConfigClasses
  :: Reflex t
  => CheckboxConfig t -> Active t Classes
checkboxConfigClasses CheckboxConfig {..} = dynClasses
  [ pure $ Just "ui checkbox"
  , fmap toClassText <$> _checkboxConfig_type
  , boolClass "fitted" _checkboxConfig_fitted
  , boolClass "disabled" _checkboxConfig_disabled
  ]

-- | Result of running a checkbox
data Checkbox t = Checkbox
  { _checkbox_value :: Dynamic t Bool
  -- ^ The current checked state
  , _checkbox_change :: Event t Bool
  -- ^ Changes which are invoked by the user
  , _checkbox_indeterminate :: Dynamic t Bool
  -- ^ The current indeterminate state
  , _checkbox_hasFocus :: Dynamic t Bool
  -- ^ The current focused state
  , _checkbox_divElement :: El t
  -- ^ The checkbox div element
  , _checkbox_inputElement :: El t
  -- ^ The checkbox input element
  }
#ifdef USE_TEMPLATE_HASKELL
makeLensesWith (lensRules & simpleLenses .~ True) ''Checkbox
#endif

instance HasValue (Checkbox t) where
  type Value (Checkbox t) = Dynamic t Bool
  value = _checkbox_value

-- | Checkbox UI Element. The minimum useful checkbox only needs a label and a
-- default configuration.
checkbox
  :: forall t m.
    ( DOM.MonadJSM m, DOM.MonadJSM (Performable m), MonadSample t (Performable m)
    , DomBuilderSpace m ~ GhcjsDomSpace, UI t m )
  => m () -> CheckboxConfig t -> m (Checkbox t)
checkbox content config@CheckboxConfig {..} = do

  let divAttrs = _checkboxConfig_elConfig <> def
        { _classes = checkboxConfigClasses config }
      constAttrs = "type" =: "checkbox" <> "class" =: "hidden"
      cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & initialAttributes .~ constAttrs
        & elementConfig_eventSpec %~ addEventSpecFlags
          (Proxy :: Proxy (DomBuilderSpace m)) Click (const stopPropagation)

  (divEl, inputEl) <- ui' "div" divAttrs $ do
    (inputEl, _) <- element "input" cfg blank
    el "label" content
    return inputEl

  let e = DOM.uncheckedCastTo DOM.HTMLInputElement $ _element_raw inputEl

  case _checkboxConfig_disabled of
    Static x -> Input.setDisabled e x
    Dyn x -> do
      -- Set initial disabled
      Input.setDisabled e <=< sample $ current x
      -- Set future disabled
      performEvent_ $ ffor (updated x) $ Input.setDisabled e

  -- Set initial value
  Input.setChecked e $ _initial _checkboxConfig_setValue
  -- Set future value, filtering uniques
  mSetEvent <- for (_event _checkboxConfig_setValue) $
    \setValue -> performEvent $ ffor setValue $ \newValue -> do
      oldValue <- liftJSM $ Input.getChecked e
      Input.setChecked e newValue
      return $ newValue <$ guard (newValue /= oldValue)

  -- Set initial indeterminate
  Input.setIndeterminate e $ _initial _checkboxConfig_setIndeterminate
  -- Set future indeterminate
  for_ (_event _checkboxConfig_setIndeterminate) $ \setIndeterminate ->
    performEvent_ $ ffor setIndeterminate $ Input.setIndeterminate e

  rec

    -- Events from the UI
    let eFlip = domEvent Change inputEl <> domEvent Click divEl
        eReset = domEvent Reset inputEl
        trigger = leftmost [True <$ eReset, False <$ eFlip]
    uiEvent <- fmap (fmapMaybe id) $ performEvent $ ffor trigger $ \reset -> do

      -- If the checkbox is disabled we do nothing
      disabled <- Input.getDisabled e
      case (disabled, reset) of
        (True, _) -> pure Nothing
        (_, True) -> pure $ Just (False, False)
        _ -> do

          -- This seems to be the only way to get the values from *before* the
          -- user interacted with the checkbox.
          oldValue <- sample $ current value
          oldIndeterminate <- sample $ current indeterminate

          -- If the checkbox was indeterminate we always set checked to true,
          -- otherwise we toggle the value.
          -- This matches the Semantic UI visual behaviour
          let newValue = oldIndeterminate || not oldValue
          -- Always clear the indeterminate state
              newIndeterminate = False

          Input.setChecked e newValue
          Input.setIndeterminate e newIndeterminate

          return $ Just (newValue, newIndeterminate)

    performEvent_ $ ffor (keydown Escape inputEl) $ \_ -> void $ runMaybeT $ do
      document <- MaybeT currentDocument
      activeElement <- MaybeT $ getActiveElement document
      htmlElement <- MaybeT $ castTo HTMLElement activeElement
      HTMLElement.blur htmlElement

    indeterminate <- holdDyn (_initial _checkboxConfig_setIndeterminate) $ leftmost
      $ fmap snd uiEvent
      : maybe [] pure (_event _checkboxConfig_setIndeterminate)

    value <- holdUniqDyn <=< holdDyn (_initial _checkboxConfig_setValue) $ leftmost
      $ fmap fst uiEvent
      : maybe [] (pure . fmapMaybe id) mSetEvent

  let initialFocus = False -- FIXME: Is this correct?
  focus <- holdDyn initialFocus $ leftmost
    [ False <$ select (_element_events inputEl) (WrapArg Blur)
    , True <$ select (_element_events inputEl) (WrapArg Focus)
    ]

  return Checkbox
    { _checkbox_value = value
    , _checkbox_change = fst <$> uiEvent
    , _checkbox_indeterminate = indeterminate
    , _checkbox_hasFocus = focus
    , _checkbox_divElement = divEl
    , _checkbox_inputElement = inputEl
    }

#ifndef USE_TEMPLATE_HASKELL
#include "Checkbox.th.hs"
#endif
