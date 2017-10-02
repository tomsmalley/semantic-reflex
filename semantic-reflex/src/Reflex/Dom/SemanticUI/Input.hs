{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE RecursiveDo              #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}

module Reflex.Dom.SemanticUI.Input where

import           Data.Default
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Reflex.Dom.Core hiding (Input, fromJSString)
import           Reflex.Dom.SemanticUI.Common
import           Reflex.Dom.SemanticUI.Icon

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
  , case _icon of NeverRender -> mempty; RenderWhen {} -> "icon"
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
