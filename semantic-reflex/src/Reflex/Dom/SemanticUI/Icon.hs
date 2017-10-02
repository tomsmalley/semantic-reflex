{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DuplicateRecordFields                      #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE RecursiveDo              #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}

module Reflex.Dom.SemanticUI.Icon where

import Data.Foldable (traverse_)
import           Data.Default
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Reflex.Dom.Core hiding (fromJSString)
import Data.Maybe (catMaybes)

import           Reflex.Dom.SemanticUI.Common

--data Flag t = Flag (Dynamic t Text)
data Flag t = Flag (Active t Text)

instance t ~ t' => UI t' m (Flag t) where
  type Return t' m (Flag t) = ()
  --ui' (Flag flagDyn) = elDynAttr' "i" (attr <$> flagDyn) blank
  ui' (Flag flagActive) = case flagActive of
    Static flag -> elAttr' "i" (attr flag) blank
    Dynamic flagDyn -> elDynAttr' "i" (attr <$> flagDyn) blank
    where
      attr flag = "class" =: (flag <> " flag")

data Icon t
  = Icon (Active t Text) (IconConfig t)
  | Icons [Icon t] IconsConfig

data IconConfig t = IconConfig
  { _disabled :: Active t Bool
  , _loading :: Active t Bool
  , _fitted :: Active t Bool
  , _size :: Active t (Maybe Size)
  , _link :: Active t Bool
  , _floated :: Active t (Maybe Floated)
  , _title :: Active t (Maybe Text)
  , _circular :: Active t Bool
--  , _flipped :: Bool
--  , _rotated :: Bool
--  , _circular :: Bool
--  , _bordered :: Bool
  , _inverted :: Active t Bool
  , _color :: Active t (Maybe Color)
  }

instance Reflex t => Default (IconConfig t) where
  def = IconConfig
    { _disabled = pure False
    , _loading = pure False
    , _fitted = pure False
    , _size = pure Nothing
    , _link = pure False
    , _floated = pure Nothing
    , _title = pure Nothing
    , _circular = pure False
    , _inverted = pure False
    , _color = pure Nothing
    }

iconConfigClasses :: Reflex t => IconConfig t -> Active t ClassText
iconConfigClasses IconConfig {..} = mconcat
  [ memptyUnless "disabled" <$> _disabled
  , memptyUnless "loading" <$> _loading
  , memptyUnless "fitted" <$> _fitted
  , memptyUnless "link" <$> _link
  , memptyUnless "circular" <$> _circular
  , memptyUnless "inverted" <$> _inverted
  , toClassText . nothingIf Medium <$> _size
  , toClassText <$> _floated
  , toClassText <$> _color
  ]

data IconsConfig = IconsConfig
  { _size :: Maybe Size
  }
  deriving (Eq, Show)

instance Default IconsConfig where
  def = IconsConfig
    { _size = Nothing
    }

instance t' ~ t => UI t' m (Icon t) where
  type Return t' m (Icon t) = ()

  ui' (Icon activeIcon config@IconConfig {..}) = elActiveAttr' "i" attrs blank
    where
      attrs = mkAttrs <$> activeIcon <*> iconConfigClasses config <*> _title
      mkAttrs i c t = maybe mempty ("title" =:) t
                   <> "class" =: getClass (mconcat [ClassText (Just i), "icon", c])

  ui' (Icons icons IconsConfig {..})
    = elAttr' "i" ("class" =: getClass classes) $ traverse_ ui_ icons
      where classes = mconcat ["icons", toClassText _size]

