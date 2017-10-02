{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE TypeFamilies             #-}

module Reflex.Dom.SemanticUI.Input where

import Data.Default
import Data.Monoid
import Data.Text (Text)
import Reflex.Dom.Core hiding (Input, fromJSString)
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Icon

data Input t = Input
  { _config :: InputConfig t
  }

data InputConfig t = InputConfig
  { _disabled :: Active t Bool
  , _icon :: RenderWhen t (Icon t)
  , _placeholder :: Active t (Maybe Text)
  }

instance Default (InputConfig t) where
  def = InputConfig
    { _disabled = Static False
    , _icon = NeverRender
    , _placeholder = Static Nothing
    }

inputConfigClasses :: Reflex t => InputConfig t -> Active t ClassText
inputConfigClasses InputConfig {..} = mconcat
  [ memptyUnless "disabled" <$> _disabled
  , if isNeverRender _icon then mempty else "icon"
  ]

data InputResult t = InputResult
  { _value :: Dynamic t Text }

instance t' ~ t => UI t' m (Input t) where
  type Return t' m (Input t) = InputResult t

  ui' (Input config@InputConfig {..}) = do
    (divEl, inputResult) <- elActiveAttr' "div" divAttrs $ do
      TextInput {..} <- textInput def
        { _textInputConfig_attributes = inputAttrs }
      runRenderWhen ui' _icon
      return _textInput_value
    return (divEl, InputResult inputResult)
    where
      divAttrs = mkDivAttrs <$> inputConfigClasses config
      mkDivAttrs c = "class" =: getClass (mconcat ["ui input", c])
      inputAttrs = active pure id $ mkInputAttrs <$> _placeholder
      mkInputAttrs mp = "type" =: "text" <> maybe mempty ("placeholder" =:) mp
