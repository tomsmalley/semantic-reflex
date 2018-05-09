{-# LANGUAGE GADTs #-}

module Reflex.Dom.SemanticUI.Input
  (

    input, input'
  , InputConfig (..)
  , InputIcon (..)
  , InputAction (..)
  , inputConfig_loading
  , inputConfig_disabled
  , inputConfig_error
  , inputConfig_transparent
  , inputConfig_inverted
  , inputConfig_fluid
  , inputConfig_icon
  , inputConfig_labeled
  , inputConfig_action
  , inputConfig_size
  , inputConfig_elConfig

  , textInput
  , InputType (..)
  , TextInputConfig (..)
  , textInputConfig_attrs
  , textInputConfig_placeholder
  , textInputConfig_type
  , textInputConfig_value

  , TextInput (..)
  , textInput_value
  , textInput_input
  , textInput_keypress
  , textInput_keydown
  , textInput_keyup
  , textInput_hasFocus

  ) where

#ifdef USE_TEMPLATE_HASKELL
import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
#else
import Control.Lens.Type
#endif

import Control.Monad (void)
import Control.Monad.Trans.Maybe
import Data.Default
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text)
import GHCJS.DOM (currentDocument)
import GHCJS.DOM.DocumentOrShadowRoot (getActiveElement)
import qualified GHCJS.DOM.Types as DOM (castTo, HTMLElement(..), Element(..), MonadJSM)
import Reflex.Dom.Core hiding (Input, SetValue, TextInputConfig, textInput)

import Reflex.Active
import qualified Reflex.Dom.Core as Reflex
import qualified GHCJS.DOM.HTMLElement as HTMLElement

import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data InputAction = LeftAction | RightAction

instance ToClassText InputAction where
  toClassText LeftAction = "left action"
  toClassText RightAction = "action"

data InputIcon = LeftIcon | RightIcon

instance ToClassText InputIcon where
  toClassText LeftIcon = "left icon"
  toClassText RightIcon = "icon"

data InputConfig t = InputConfig
  { _inputConfig_loading     :: Active t Bool
  , _inputConfig_disabled    :: Active t Bool
  , _inputConfig_error       :: Active t Bool
  , _inputConfig_transparent :: Active t Bool
  , _inputConfig_inverted    :: Active t Bool
  , _inputConfig_fluid       :: Active t Bool

  , _inputConfig_icon        :: Active t (Maybe InputIcon)
  , _inputConfig_labeled     :: Active t (Maybe Labeled)
  , _inputConfig_action      :: Active t (Maybe InputAction)
  , _inputConfig_size        :: Active t (Maybe Size)

  , _inputConfig_elConfig    :: ActiveElConfig t
  }
#ifdef USE_TEMPLATE_HASKELL
makeLensesWith (lensRules & simpleLenses .~ True) ''InputConfig
#endif

instance HasElConfig t (InputConfig t) where
  elConfig = inputConfig_elConfig

instance Reflex t => Default (InputConfig t) where
  def = InputConfig
    { _inputConfig_loading = pure False
    , _inputConfig_disabled = pure False
    , _inputConfig_error = pure False
    , _inputConfig_icon = pure Nothing
    , _inputConfig_labeled = pure Nothing
    , _inputConfig_action = pure Nothing
    , _inputConfig_transparent = pure False
    , _inputConfig_inverted = pure False
    , _inputConfig_fluid = pure False
    , _inputConfig_size = pure Nothing
    , _inputConfig_elConfig = def
    }

inputConfigClasses :: Reflex t => InputConfig t -> Active t Classes
inputConfigClasses InputConfig {..} = dynClasses
  [ pure $ Just "ui input"
  , boolClass "loading" _inputConfig_loading
  , boolClass "disabled" _inputConfig_disabled
  , boolClass "error" _inputConfig_error
  , fmap toClassText <$> _inputConfig_icon
  , fmap toClassText <$> _inputConfig_labeled
  , fmap toClassText <$> _inputConfig_action
  , boolClass "transparent" _inputConfig_transparent
  , boolClass "inverted" _inputConfig_inverted
  , boolClass "fluid" _inputConfig_fluid
  -- Tiny isn't specified for some reason
  , fmap (\s -> toClassText $ if s == Tiny then Mini else s) <$> _inputConfig_size
  ]

-- | A wrapper around the reflex-dom 'textInput' which conforms to the style
-- of this library
textInput
  :: (DomBuilderSpace m ~ GhcjsDomSpace, DomBuilder t m, PostBuild t m, PerformEvent t m, DOM.MonadJSM (Performable m))
  => TextInputConfig t -> m (TextInput t)
textInput TextInputConfig {..} = do
  ti <- Reflex.textInput Reflex.TextInputConfig
    { _textInputConfig_attributes = a
    , _textInputConfig_setValue = fromMaybe never mSetValue
    , _textInputConfig_initialValue = initialValue
    , _textInputConfig_inputType = inputTypeText _textInputConfig_type
    }
  -- Blur on escape
  performEvent_ $ ffor (keydown Escape ti) $ \_ -> void $ runMaybeT $ do
    document <- MaybeT currentDocument
    activeElement <- MaybeT $ getActiveElement document
    htmlElement <- MaybeT $ DOM.castTo DOM.HTMLElement activeElement
    HTMLElement.blur htmlElement
  pure ti
  where a = (\p -> ("placeholder" =: p <>)) <$> _textInputConfig_placeholder <*> _textInputConfig_attrs
        SetValue initialValue mSetValue = _textInputConfig_value

data InputType = PlainTextInput | PasswordInput
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

inputTypeText :: InputType -> Text
inputTypeText PasswordInput = "password"
inputTypeText PlainTextInput = "text"

-- TODO: attrs needs merging with the attributes lens
data TextInputConfig t = TextInputConfig
  { _textInputConfig_value :: SetValue t Text
  , _textInputConfig_placeholder :: Dynamic t Text
  , _textInputConfig_type :: InputType
  , _textInputConfig_attrs :: Dynamic t (Map Text Text)
  }
#ifdef USE_TEMPLATE_HASKELL
makeLensesWith (lensRules & simpleLenses .~ True) ''TextInputConfig
#endif

instance Reflex t => Default (TextInputConfig t) where
  def = TextInputConfig
    { _textInputConfig_value = SetValue "" Nothing
    , _textInputConfig_placeholder = pure ""
    , _textInputConfig_type = PlainTextInput
    , _textInputConfig_attrs = pure mempty
    }

input'
  :: UI t m
  => InputConfig t -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
input' config@InputConfig {..} = ui' "div" elConf
  where elConf = _inputConfig_elConfig <> def { _classes = inputConfigClasses config }

input :: UI t m => InputConfig t -> m a -> m a
input c = fmap snd . input' c

#ifndef USE_TEMPLATE_HASKELL
#include "Input.th.hs"
#endif
