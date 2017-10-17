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
{-# LANGUAGE GeneralizedNewtypeDeriving     #-}
{-# LANGUAGE GADTs     #-}

module Reflex.Dom.SemanticUI.Button where

import Data.Default
import Data.Semigroup hiding (First)
import Data.Text (Text)
import Reflex

import Reflex.Dom.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data Conditional t = Conditional (ConditionalConfig t)
data ConditionalConfig t = ConditionalConfig
  { _dataText :: Active t (Maybe Text)
  }
instance Reflex t => Default (ConditionalConfig t) where
  def = ConditionalConfig
    { _dataText = pure Nothing
    }

data Buttons t m a = Buttons (ButtonsConfig t) (Component Buttons m a)

data ButtonsConfig t = ButtonsConfig
  { _color :: Active t (Maybe Color)
  , _size :: Active t (Maybe Size)
  , _basic :: Active t Bool
  , _icon :: Active t Bool
  , _labeledIcon :: Active t Bool
  , _vertical :: Active t Bool
  , _compact :: Active t Bool
  , _attached :: Active t (Maybe VerticalAttached)
  , _width :: Active t (Maybe Width)
  , _floated :: Active t (Maybe Floated)
  , _config :: ActiveElConfig t
  }

instance Reflex t => Default (ButtonsConfig t) where
  def = ButtonsConfig
    { _color = pure Nothing
    , _size = pure Nothing
    , _basic = pure False
    , _icon = pure False
    , _labeledIcon = pure False
    , _vertical = pure False
    , _compact = pure False
    , _attached = pure Nothing
    , _width = pure Nothing
    , _floated = pure Nothing
    , _config = def
    }

buttonsConfigClasses :: Reflex t => ButtonsConfig t -> Active t Classes
buttonsConfigClasses ButtonsConfig {..} = activeClasses
  [ Static $ Just "ui buttons"
  , boolClass "basic" _basic
  , boolClass "icon" _icon
  , boolClass "labeled icon" _labeledIcon
  , boolClass "vertical" _vertical
  , boolClass "compact" _compact
  , fmap toClassText <$> _color
  , fmap toClassText <$> _size
  , fmap toClassText <$> _attached
  , fmap toClassText <$> _width
  , fmap toClassText <$> _floated
  ]


data Button t m = Button
  { _config :: ButtonConfig t m
  , _content :: Component Button m ()
  }

data DivButton t m = DivButton
  { _config :: ButtonConfig t m
  , _content :: Component Button m ()
  }

data LabeledButton t m = LabeledButton
  { _config :: LabeledButtonConfig t
  , _content :: Component LabeledButton m ()
  }

data Labeled = LeftLabeled | RightLabeled

instance ToClassText Labeled where
  toClassText LeftLabeled = "left labeled"
  toClassText RightLabeled = "right labeled"

data LabeledButtonConfig t = LabeledButtonConfig
  { _labeled :: Active t Labeled
  , _config :: ActiveElConfig t
  }

instance Reflex t => Default (LabeledButtonConfig t) where
  def = LabeledButtonConfig
    { _labeled = pure RightLabeled
    , _config = def
    }

labeledButtonConfigClasses :: Reflex t => LabeledButtonConfig t -> Active t Classes
labeledButtonConfigClasses LabeledButtonConfig {..} = activeClasses
  [ Static $ Just "ui button"
  , Just . toClassText <$> _labeled
  ]

data AnimatedButtonType = Animated | VerticalAnimated | AnimatedFade
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToClassText AnimatedButtonType where
  toClassText Animated = "animated"
  toClassText VerticalAnimated = "vertical animated"
  toClassText AnimatedFade = "animated fade"

data AnimatedButton t m = AnimatedButton
  { _animatedType :: Active t AnimatedButtonType
  , _content :: Component Button m ()
  }

instance Monad m => Default (AnimatedButton t m) where
  def = AnimatedButton
    { _animatedType = Static Animated
    , _content = pure ()
    }

data ButtonConfig t m = ButtonConfig
  { _color :: Active t (Maybe Color)
  , _size :: Active t (Maybe Size)
  , _emphasis :: Active t (Maybe Emphasis)
  , _positive :: Active t (Maybe Positive)
  , _social :: Active t (Maybe Social)
  , _floated :: Active t (Maybe Floated)
  , _disabled :: Active t Bool
  , _compact :: Active t Bool
  , _basic :: Active t Bool
  , _icon :: Active t Bool
  , _inverted :: Active t Bool
  , _loading :: Active t Bool
  , _fluid :: Active t Bool
  , _circular :: Active t Bool
  , _labeledIcon :: Active t (Maybe Labeled)
  , _attached :: Active t (Maybe ExclusiveAttached)
  , _animated :: Maybe (AnimatedButton t m)
  , _config :: ActiveElConfig t
  }

instance Reflex t => Default (ButtonConfig t m) where
  def = ButtonConfig
    { _color = pure Nothing
    , _size = pure Nothing
    , _emphasis = pure Nothing
    , _positive = pure Nothing
    , _social = pure Nothing
    , _floated = pure Nothing
    , _disabled = pure False
    , _compact = pure False
    , _basic = pure False
    , _icon = pure False
    , _inverted = pure False
    , _loading = pure False
    , _fluid = pure False
    , _circular = pure False
    , _labeledIcon = pure Nothing
--    , _icon = NeverRender
    , _attached = pure Nothing
    , _animated = Nothing
    , _config = def
    }

-- | Change success to positive and error to negative, since button does not
-- support success/error types
filterPositive :: Positive -> Positive
filterPositive Positive = Positive
filterPositive Negative = Negative
filterPositive Success = Positive
filterPositive Error = Negative

buttonConfigClasses :: Reflex t => ButtonConfig t m -> Active t Classes
buttonConfigClasses ButtonConfig {..} = activeClasses
  [ Static $ Just "ui button"
  , boolClass "disabled" _disabled
  , boolClass "compact" _compact
  , boolClass "basic" _basic
  , boolClass "icon" _icon
  , boolClass "inverted" _inverted
  , boolClass "loading" _loading
  , boolClass "fluid" _fluid
  , boolClass "circular" _circular
  , fmap ((<> " icon") . toClassText) <$> _labeledIcon
  , fmap toClassText <$> _color
  , fmap toClassText <$> _size
  , fmap toClassText <$> _emphasis
  , fmap (toClassText . filterPositive) <$> _positive
  , fmap toClassText <$> _social
  , fmap toClassText <$> _floated
  , fmap toClassText <$> _attached
  , traverse (fmap toClassText . _animatedType) _animated
  ]
