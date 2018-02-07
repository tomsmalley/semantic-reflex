{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Semantic UI checkboxes. Pure reflex implementation is provided.
-- https://semantic-ui.com/modules/checkbox.html
-- TODO:
-- - Read-only and uncheckable states
-- - Blur on escape key
-- - Toggle on enter key
module Reflex.Dom.SemanticUI.Checkbox where

import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
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
import Reflex.Dom.Core hiding (CheckboxConfig, Checkbox, SetValue(..))

import Reflex.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

import qualified GHCJS.DOM.HTMLElement as HTMLElement
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.HTMLInputElement as Input
import Language.Javascript.JSaddle (liftJSM)

-- | Checkbox types. If you need a radio type, see <RadioGroup>.
data CheckboxType =  Slider | Toggle deriving (Eq, Show)

instance ToClassText CheckboxType where
  toClassText Slider = "slider"
  toClassText Toggle = "toggle"

-- | Configuration of a checkbox. Value and indeterminate are split into initial
-- and set events in order to logically disconnect them from their dynamic
-- return values in Checkbox.
data CheckboxConfig t = CheckboxConfig
  { _checkboxSetValue :: SetValue t Bool
  -- ^ Value control

  , _checkboxSetIndeterminate :: SetValue t Bool
  -- ^ Control over indeterminate state

  , _checkboxType :: Active t (Maybe CheckboxType)
  -- ^ Checkbox type, e.g. slider
  , _checkboxFitted :: Active t Bool
  -- ^ Checkbox is fitted
  , _checkboxDisabled :: Active t Bool
  -- ^ Checkbox is disabled

  , _checkboxElConfig :: ActiveElConfig t
  -- ^ Config
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''CheckboxConfig

instance HasElConfig t (CheckboxConfig t) where
  elConfig = checkboxElConfig

instance Reflex t
  => Default (CheckboxConfig t) where
  def = CheckboxConfig
    { _checkboxSetValue = SetValue False Nothing
    , _checkboxSetIndeterminate = SetValue False Nothing

    , _checkboxType = pure Nothing
    , _checkboxFitted = pure False
    , _checkboxDisabled = pure False
    , _checkboxElConfig = def
    }

-- | Make the checkbox div classes from the configuration
checkboxConfigClasses
  :: Reflex t
  => CheckboxConfig t -> Active t Classes
checkboxConfigClasses CheckboxConfig {..} = dynClasses
  [ pure $ Just "ui checkbox"
  , fmap toClassText <$> _checkboxType
  , boolClass "fitted" _checkboxFitted
  , boolClass "disabled" _checkboxDisabled
  ]

-- | Result of running a checkbox
data Checkbox t = Checkbox
  { _checkboxValue :: Dynamic t Bool
  -- ^ The current checked state
  , _checkboxChange :: Event t Bool
  -- ^ Changes which are invoked by the user
  , _checkboxIsIndeterminate :: Dynamic t Bool
  -- ^ The current indeterminate state
  , _checkboxHasFocus :: Dynamic t Bool
  -- ^ The current focused state
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''Checkbox

-- | Checkbox UI Element. The minimum useful checkbox only needs a label and a
-- default configuration.
checkbox :: MonadWidget t m => m () -> CheckboxConfig t -> m (Checkbox t)
checkbox m = fmap snd . checkbox' m

-- | Checkbox UI Element. The minimum useful checkbox only needs a label and a
-- default configuration.
checkbox'
  :: forall t m. MonadWidget t m
  => m () -> CheckboxConfig t -> m (El t, Checkbox t)
checkbox' content config@CheckboxConfig {..} = do

  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & initialAttributes .~ constAttrs
        & elementConfig_eventSpec %~ addEventSpecFlags
          (Proxy :: Proxy (DomBuilderSpace m)) Click (const stopPropagation)

  (divEl, inputEl) <- uiElement' "div" divAttrs $ do
    (inputEl, _) <- element "input" cfg blank
    el "label" content
    return inputEl

  let e = DOM.uncheckedCastTo DOM.HTMLInputElement $ _element_raw inputEl

  case _checkboxDisabled of
    Static x -> Input.setDisabled e x
    Dyn x -> do
      -- Set initial disabled
      Input.setDisabled e <=< sample $ current x
      -- Set future disabled
      performEvent_ $ ffor (updated x) $ Input.setDisabled e

  -- Set initial value
  Input.setChecked e $ _initial _checkboxSetValue
  -- Set future value, filtering uniques
  mSetEvent <- for (_event _checkboxSetValue) $
    \setValue -> performEvent $ ffor setValue $ \newValue -> do
      oldValue <- liftJSM $ Input.getChecked e
      Input.setChecked e newValue
      return $ newValue <$ guard (newValue /= oldValue)

  -- Set initial indeterminate
  Input.setIndeterminate e $ _initial _checkboxSetIndeterminate
  -- Set future indeterminate
  for_ (_event _checkboxSetIndeterminate) $ \setIndeterminate ->
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

    indeterminate <- holdDyn (_initial _checkboxSetIndeterminate) $ leftmost
      $ fmap snd uiEvent
      : maybe [] pure (_event _checkboxSetIndeterminate)

    value <- holdUniqDyn <=< holdDyn (_initial _checkboxSetValue) $ leftmost
      $ fmap fst uiEvent
      : maybe [] (pure . fmapMaybe id) mSetEvent

  let initialFocus = False -- FIXME: Is this correct?
  focus <- holdDyn initialFocus $ leftmost
    [ False <$ select (_element_events inputEl) (WrapArg Blur)
    , True <$ select (_element_events inputEl) (WrapArg Focus)
    ]

  return (divEl, Checkbox
    { _checkboxValue = value
    , _checkboxChange = fst <$> uiEvent
    , _checkboxIsIndeterminate = indeterminate
    , _checkboxHasFocus = focus
    })

  where
    divAttrs = _checkboxElConfig <> def
      { _classes = checkboxConfigClasses config
      }
    constAttrs = "type" =: "checkbox" <> "class" =: "hidden"

