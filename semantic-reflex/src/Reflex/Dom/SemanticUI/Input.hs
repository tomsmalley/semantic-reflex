{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Reflex.Dom.SemanticUI.Input
  (

    input, input'
  , InputConfig (..)
  , inputLoading
  , inputDisabled
  , inputHasError
  , inputTransparent
  , inputInverted
  , inputFluid
  , inputIconSide
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

  ) where

import Control.Lens (makeLenses)
import Data.Default
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Reflex.Dom.Core hiding (Input, SetValue, TextInputConfig, textInput)
import qualified Reflex.Dom.Core as Reflex

import Reflex.Dom.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data Action = LeftAction | RightAction

instance ToClassText Action where
  toClassText LeftAction = "left action"
  toClassText RightAction = "action"

data InputIcon = LeftIcon | RightIcon

instance ToClassText InputIcon where
  toClassText LeftIcon = "left icon"
  toClassText RightIcon = "icon"

data InputConfig t = InputConfig
  { _inputLoading     :: Active t Bool
  , _inputDisabled    :: Active t Bool
  , _inputHasError    :: Active t Bool
  , _inputTransparent :: Active t Bool
  , _inputInverted    :: Active t Bool
  , _inputFluid       :: Active t Bool

  , _inputIconSide    :: Active t (Maybe InputIcon)
  , _inputLabeled     :: Active t (Maybe Labeled)
  , _inputAction      :: Active t (Maybe Action)
  , _inputSize        :: Active t (Maybe Size)

  , _inputElConfig    :: ActiveElConfig t
  }
makeLenses ''InputConfig

instance Reflex t => Default (InputConfig t) where
  def = InputConfig
    { _inputLoading = pure False
    , _inputDisabled = pure False
    , _inputHasError = pure False
    , _inputIconSide = pure Nothing
    , _inputLabeled = pure Nothing
    , _inputAction = pure Nothing
    , _inputTransparent = pure False
    , _inputInverted = pure False
    , _inputFluid = pure False
    , _inputSize = pure Nothing
    , _inputElConfig = def
    }

inputConfigClasses :: Reflex t => InputConfig t -> Active t Classes
inputConfigClasses InputConfig {..} = activeClasses
  [ Static $ Just "ui input"
  , boolClass "loading" _inputLoading
  , boolClass "disabled" _inputDisabled
  , boolClass "error" _inputHasError
  , fmap toClassText <$> _inputIconSide
  , fmap toClassText <$> _inputLabeled
  , fmap toClassText <$> _inputAction
  , boolClass "transparent" _inputTransparent
  , boolClass "inverted" _inputInverted
  , boolClass "fluid" _inputFluid
  -- Tiny isn't specified for some reason
  , fmap (\s -> toClassText $ if s == Tiny then Mini else s) <$> _inputSize
  ]

{-
instance DynShow t (TextInput t) where
  dynShow TextInput {..} = do
    input <- countWithLast _textInput_input
    keypress <- countWithLast _textInput_keypress
    keydown <- countWithLast _textInput_keydown
    keyup <- countWithLast _textInput_keyup
    return $ mconcat
      [ pure "TextInputResult"
      , (("\n  { _value = " <>) . show) <$> _textInput_value
      , (("\n  , _input = " <>) . show) <$> input
      , (("\n  , _keypress = " <>) . show) <$> keypress
      , (("\n  , _keydown = " <>) . show) <$> keydown
      , (("\n  , _keyup = " <>) . show) <$> keyup
      , (("\n  , _focus = " <>) . show) <$> _textInput_focus
      , pure "\n  }"
      ]
-}

-- | A wrapper around the reflex-dom 'textInput' which conforms to the style
-- of this library
textInput :: (DomBuilder t m, PostBuild t m, DomBuilderSpace m ~ GhcjsDomSpace)
          => TextInputConfig t -> m (TextInput t)
textInput TextInputConfig {..}
  = Reflex.textInput Reflex.TextInputConfig
    { _textInputConfig_attributes = a
    , _textInputConfig_setValue = fromMaybe never mSetValue
    , _textInputConfig_initialValue = initialValue
    , _textInputConfig_inputType = inputTypeText _textInputType
    }
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
makeLenses ''TextInputConfig

instance Reflex t => Default (TextInputConfig t) where
  def = TextInputConfig
    { _textInputValue = SetValue "" Nothing
    , _textInputPlaceholder = pure ""
    , _textInputType = PlainTextInput
    , _textInputAttrs = pure mempty
    }

input' :: MonadWidget t m => InputConfig t -> m a -> m (El t, a)
input' config@InputConfig {..} = element' "div" elConf
  where elConf = _inputElConfig <> def { _classes = inputConfigClasses config }

input :: MonadWidget t m => InputConfig t -> m a -> m a
input c = fmap snd . input' c

