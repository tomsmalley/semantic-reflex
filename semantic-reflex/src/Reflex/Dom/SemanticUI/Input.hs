{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE TypeFamilies             #-}

module Reflex.Dom.SemanticUI.Input where

import Data.Default
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Reflex.Dom.Core hiding (Input, SetValue, TextInput, TextInputConfig)
import qualified Reflex.Dom.Core as Reflex

import Reflex.Dom.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition
import Reflex.Dom.SemanticUI.Icon

data Input t m a = Input
  { _config :: InputConfig t
  , _contents :: Component Input m a
  }

data Action = LeftAction | RightAction

instance ToClassText Action where
  toClassText LeftAction = "left action"
  toClassText RightAction = "action"

data InputIcon = LeftIcon | RightIcon

instance ToClassText InputIcon where
  toClassText LeftIcon = "left icon"
  toClassText RightIcon = "icon"

data InputConfig t = InputConfig
  { _loading :: Active t Bool
  , _disabled :: Active t Bool
  , _hasError :: Active t Bool
  , _icon :: Active t (Maybe InputIcon)
  , _labeled :: Active t (Maybe Labeled)
  , _action :: Active t (Maybe Action)
  , _transparent :: Active t Bool
  , _inverted :: Active t Bool
  , _fluid :: Active t Bool
  , _size :: Active t (Maybe Size)
  , _config :: ActiveElConfig t
  }

instance Reflex t => Default (InputConfig t) where
  def = InputConfig
    { _loading = pure False
    , _disabled = pure False
    , _hasError = pure False
    , _icon = pure Nothing
    , _labeled = pure Nothing
    , _action = pure Nothing
    , _transparent = pure False
    , _inverted = pure False
    , _fluid = pure False
    , _size = pure Nothing
    , _config = def
    }

inputConfigClasses :: Reflex t => InputConfig t -> Active t Classes
inputConfigClasses InputConfig {..} = activeClasses
  [ Static $ Just "ui input"
  , boolClass "loading" _loading
  , boolClass "disabled" _disabled
  , boolClass "error" _hasError
  , fmap toClassText <$> _icon
  , fmap toClassText <$> _labeled
  , fmap toClassText <$> _action
  , boolClass "transparent" _transparent
  , boolClass "inverted" _inverted
  , boolClass "fluid" _fluid
  -- Tiny isn't specified for some reason
  , fmap (\s -> toClassText $ if s == Tiny then Mini else s) <$> _size
  ]

data TextInputResult t = TextInputResult
  { _value :: Dynamic t Text
  , _input :: Event t Text
  , _keypress :: Event t Word
  , _keydown :: Event t Word
  , _keyup :: Event t Word
  , _focus :: Dynamic t Bool
  , _builderElement :: InputElement EventResult GhcjsDomSpace t
  }

instance DynShow t (TextInputResult t) where
  dynShow TextInputResult {..} = do
    input <- countWithLast _input
    keypress <- countWithLast _keypress
    keydown <- countWithLast _keydown
    keyup <- countWithLast _keyup
    return $ mconcat
      [ pure "TextInputResult"
      , (("\n  { _value = " <>) . show) <$> _value
      , (("\n  , _input = " <>) . show) <$> input
      , (("\n  , _keypress = " <>) . show) <$> keypress
      , (("\n  , _keydown = " <>) . show) <$> keydown
      , (("\n  , _keyup = " <>) . show) <$> keyup
      , (("\n  , _focus = " <>) . show) <$> _focus
      , pure "\n  }"
      ]

-- | A wrapper around the reflex-dom 'textInput' which conforms to the style
-- of this library
textInput :: (DomBuilder t m, PostBuild t m, DomBuilderSpace m ~ GhcjsDomSpace)
          => TextInputConfig t -> Component Input m (TextInputResult t)
textInput TextInputConfig {..} = Component $ do
  Reflex.TextInput {..} <- Reflex.textInput Reflex.TextInputConfig
    { _textInputConfig_attributes = attrs
    , _textInputConfig_setValue = fromMaybe never mSetValue
    , _textInputConfig_initialValue = initialValue
    , _textInputConfig_inputType = inputTypeText _inputType
    }
  return TextInputResult
    { _value = _textInput_value
    , _input = _textInput_input
    , _keypress = _textInput_keypress
    , _keydown = _textInput_keydown
    , _keyup = _textInput_keyup
    , _focus = _textInput_hasFocus
    , _builderElement = _textInput_builderElement
    }

      where attrs = (\p -> ("placeholder" =: p <>)) <$> _placeholder <*> _attrs
            SetValue initialValue mSetValue = _value

data InputType = TextInput | PasswordInput
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

inputTypeText :: InputType -> Text
inputTypeText PasswordInput = "password"
inputTypeText TextInput = "text"

-- | TODO: attrs needs merging with the attributes lens
data TextInputConfig t = TextInputConfig
  { _value :: SetValue t Text
  , _placeholder :: Dynamic t Text
  , _inputType :: InputType
  , _attrs :: Dynamic t (Map Text Text)
  }

instance Reflex t => Default (TextInputConfig t) where
  def = TextInputConfig
    { _value = SetValue "" Nothing
    , _placeholder = pure ""
    , _inputType = TextInput
    , _attrs = pure mempty
    }

