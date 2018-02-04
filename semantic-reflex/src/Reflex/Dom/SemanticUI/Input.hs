{-# LANGUAGE TemplateHaskell #-}

module Reflex.Dom.SemanticUI.Input
  (

    input, input'
  , InputConfig (..)
  , InputIcon (..)
  , InputAction (..)
  , inputLoading
  , inputDisabled
  , inputError
  , inputTransparent
  , inputInverted
  , inputFluid
  , inputIcon
  , inputLabeled
  , inputAction
  , inputSize
  , inputElConfig

  , textInput
  , InputType (..)
  , TextInputConfig (..)
  , textInputAttrs
  , textInputPlaceholder
  , textInputType
  , textInputValue

  , TextInput (..)
  , textInput_value
  , textInput_input
  , textInput_keypress
  , textInput_keydown
  , textInput_keyup
  , textInput_hasFocus

  ) where

import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
import Control.Monad (void)
import Control.Monad.Trans.Maybe
import Data.Default
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text)
import GHCJS.DOM (currentDocument)
import GHCJS.DOM.DocumentOrShadowRoot (getActiveElement)
import GHCJS.DOM.Types (castTo, HTMLElement(..), Element(..))
import Reflex.Dom.Core hiding (Input, SetValue, TextInputConfig, textInput)

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
  { _inputLoading     :: Dynamic t Bool
  , _inputDisabled    :: Dynamic t Bool
  , _inputError       :: Dynamic t Bool
  , _inputTransparent :: Dynamic t Bool
  , _inputInverted    :: Dynamic t Bool
  , _inputFluid       :: Dynamic t Bool

  , _inputIcon        :: Dynamic t (Maybe InputIcon)
  , _inputLabeled     :: Dynamic t (Maybe Labeled)
  , _inputAction      :: Dynamic t (Maybe InputAction)
  , _inputSize        :: Dynamic t (Maybe Size)

  , _inputElConfig    :: ActiveElConfig t
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''InputConfig

instance Reflex t => Default (InputConfig t) where
  def = InputConfig
    { _inputLoading = pure False
    , _inputDisabled = pure False
    , _inputError = pure False
    , _inputIcon = pure Nothing
    , _inputLabeled = pure Nothing
    , _inputAction = pure Nothing
    , _inputTransparent = pure False
    , _inputInverted = pure False
    , _inputFluid = pure False
    , _inputSize = pure Nothing
    , _inputElConfig = def
    }

inputConfigClasses :: Reflex t => InputConfig t -> Dynamic t Classes
inputConfigClasses InputConfig {..} = dynClasses
  [ pure $ Just "ui input"
  , boolClass "loading" _inputLoading
  , boolClass "disabled" _inputDisabled
  , boolClass "error" _inputError
  , fmap toClassText <$> _inputIcon
  , fmap toClassText <$> _inputLabeled
  , fmap toClassText <$> _inputAction
  , boolClass "transparent" _inputTransparent
  , boolClass "inverted" _inputInverted
  , boolClass "fluid" _inputFluid
  -- Tiny isn't specified for some reason
  , fmap (\s -> toClassText $ if s == Tiny then Mini else s) <$> _inputSize
  ]

-- | A wrapper around the reflex-dom 'textInput' which conforms to the style
-- of this library
textInput :: MonadWidget t m => TextInputConfig t -> m (TextInput t)
textInput TextInputConfig {..} = do
  ti <- Reflex.textInput Reflex.TextInputConfig
    { _textInputConfig_attributes = a
    , _textInputConfig_setValue = fromMaybe never mSetValue
    , _textInputConfig_initialValue = initialValue
    , _textInputConfig_inputType = inputTypeText _textInputType
    }
  -- Blur on escape
  performEvent_ $ ffor (keydown Escape ti) $ \_ -> void $ runMaybeT $ do
    document <- MaybeT currentDocument
    activeElement <- MaybeT $ getActiveElement document
    htmlElement <- MaybeT $ castTo HTMLElement activeElement
    HTMLElement.blur htmlElement
  pure ti
  where a = (\p -> ("placeholder" =: p <>)) <$> _textInputPlaceholder <*> _textInputAttrs
        SetValue initialValue mSetValue = _textInputValue

data InputType = PlainTextInput | PasswordInput
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

inputTypeText :: InputType -> Text
inputTypeText PasswordInput = "password"
inputTypeText PlainTextInput = "text"

-- TODO: attrs needs merging with the attributes lens
data TextInputConfig t = TextInputConfig
  { _textInputValue :: SetValue t Text
  , _textInputPlaceholder :: Dynamic t Text
  , _textInputType :: InputType
  , _textInputAttrs :: Dynamic t (Map Text Text)
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''TextInputConfig

instance Reflex t => Default (TextInputConfig t) where
  def = TextInputConfig
    { _textInputValue = SetValue "" Nothing
    , _textInputPlaceholder = pure ""
    , _textInputType = PlainTextInput
    , _textInputAttrs = pure mempty
    }

input' :: MonadWidget t m => InputConfig t -> m a -> m (El t, a)
input' config@InputConfig {..} = uiElement' "div" elConf
  where elConf = _inputElConfig <> def { _classes = inputConfigClasses config }

input :: MonadWidget t m => InputConfig t -> m a -> m a
input c = fmap snd . input' c

