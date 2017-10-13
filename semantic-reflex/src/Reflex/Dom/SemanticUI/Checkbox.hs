{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE RecursiveDo            #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}

-- | Semantic UI checkboxes. Pure reflex implementation is provided.
-- https://semantic-ui.com/modules/checkbox.html
-- TODO:
-- - Read-only and uncheckable states
-- - Blur on escape key
-- - Toggle on enter key
module Reflex.Dom.SemanticUI.Checkbox where

import Control.Lens ((%~))
import Control.Monad ((<=<))
import Data.Default (Default(..))
import Data.Functor.Misc (WrapArg(..))
import Data.Proxy (Proxy(..))
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.HTMLInputElement as Input
import Language.Javascript.JSaddle (liftJSM)
import Reflex
import Reflex.Dom.Core hiding (checkbox, Checkbox, CheckboxConfig)

import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

-- | Checkbox types. If you need a radio type, see <RadioGroup>.
data CheckboxType =  Slider | Toggle deriving (Eq, Show)

instance ToClassText CheckboxType where
  toClassText Slider = "slider"
  toClassText Toggle = "toggle"

-- | Configuration of a checkbox. Value and indeterminate are split into initial
-- and set events in order to logically disconnect them from their dynamic
-- return values in CheckboxResult.
data CheckboxConfig t = CheckboxConfig
  { _initialValue :: Bool
  -- ^ Initial value of checkbox
  , _setValue :: Event t Bool
  -- ^ Event which sets the value

  , _initialIndeterminate :: Bool
  -- ^ Initial indeterminate state
  , _setIndeterminate :: Event t Bool
  -- ^ Event which sets if the checkbox is indeterminate

  , _altType :: Active t (Maybe CheckboxType)
  -- ^ Checkbox type, e.g. slider
  , _fitted :: Active t Bool
  -- ^ Checkbox is fitted
  , _disabled :: Active t Bool
  -- ^ Checkbox is disabled
  , _config :: ActiveElConfig t
  -- ^ Config
  }

instance Reflex t => Default (CheckboxConfig t) where
  def = CheckboxConfig
    { _initialValue = False
    , _setValue = never

    , _initialIndeterminate = False
    , _setIndeterminate = never

    , _altType = Static Nothing
    , _fitted = Static False
    , _disabled = Static False
    , _config = def
    }

-- | Make the checkbox div classes from the configuration
checkboxConfigClasses :: Reflex t => CheckboxConfig t -> Active t Classes
checkboxConfigClasses CheckboxConfig {..} = activeClasses
  [ Static $ Just "ui checkbox"
  , fmap toClassText <$> _altType
  , boolClass "fitted" _fitted
  , boolClass "disabled" _disabled
  ]

-- | Result of running a checkbox
data CheckboxResult t = CheckboxResult
  { _value :: Dynamic t Bool
  -- ^ The current checked state
  , _change :: Event t Bool
  -- ^ Changes which are invoked by the user
  , _indeterminate :: Dynamic t Bool
  -- ^ The current indeterminate state
  , _focus :: Dynamic t Bool
  -- ^ The current focused state
  }

instance DynShow t (CheckboxResult t) where
  dynShow CheckboxResult {..} = do
    change <- countWithLast _change
    return $ mconcat
      [ pure "CheckboxResult"
      , (("\n  { _value = " <>) . show) <$> _value
      , (("\n  , _change = " <>) . show) <$> change
      , (("\n  , _indeterminate = " <>) . show) <$> _indeterminate
      , (("\n  , _focus = " <>) . show) <$> _focus
      , pure "\n  }"
      ]

-- | Checkbox UI Element. The minimum useful checkbox only needs a label and a
-- default configuration.
data Checkbox t = Checkbox
  { _label :: Active t Text
  , _config :: CheckboxConfig t
  }
