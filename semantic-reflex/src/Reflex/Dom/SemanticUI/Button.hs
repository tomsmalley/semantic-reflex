module Reflex.Dom.SemanticUI.Button
  (

  -- * Normal buttons
    button, button'
  , ButtonType (..)
  , ButtonConfig (..)
  , buttonConfig_color
  , buttonConfig_size
  , buttonConfig_emphasis
  , buttonConfig_positive
  , buttonConfig_social
  , buttonConfig_floated
  , buttonConfig_disabled
  , buttonConfig_compact
  , buttonConfig_basic
  , buttonConfig_icon
  , buttonConfig_inverted
  , buttonConfig_loading
  , buttonConfig_fluid
  , buttonConfig_circular
  , buttonConfig_labeledIcon
  , buttonConfig_attached
  , buttonConfig_animated
  , buttonConfig_type
  , buttonConfig_elConfig

  , buttonConfigClasses

  -- * Labeled buttons
  , labeledButton, labeledButton'
  , LabeledButtonConfig (..)
  , labeledButtonConfig_side
  , labeledButtonConfig_elConfig

  -- * Conditionals
  , conditional, conditional'
  , conditionalWithText, conditionalWithText'

  -- * Animated button config
  , AnimatedButtonType (..)
  , AnimatedButton (..)
  , animatedButton_hiddenContent
  , animatedButton_type

  -- * Groups of buttons
  , buttons, buttons'
  , ButtonsConfig (..)
  , buttonsConfig_color
  , buttonsConfig_size
  , buttonsConfig_basic
  , buttonsConfig_icon
  , buttonsConfig_labeledIcon
  , buttonsConfig_vertical
  , buttonsConfig_compact
  , buttonsConfig_attached
  , buttonsConfig_width
  , buttonsConfig_floated
  , buttonsConfig_elConfig

  ) where

#ifdef USE_TEMPLATE_HASKELL
import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
#else
import Control.Lens.Type
#endif

import Control.Monad (void)
import Data.Default
import Data.Semigroup hiding (First)
import Data.Text (Text)
import Reflex
import Reflex.Dom.Core hiding (Error, button)

import Reflex.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data ButtonsConfig t = ButtonsConfig
  { _buttonsConfig_basic :: Active t Bool
  , _buttonsConfig_icon :: Active t Bool
  , _buttonsConfig_labeledIcon :: Active t Bool
  , _buttonsConfig_vertical :: Active t Bool
  , _buttonsConfig_compact :: Active t Bool

  , _buttonsConfig_color :: Active t (Maybe Color)
  , _buttonsConfig_size :: Active t (Maybe Size)
  , _buttonsConfig_attached :: Active t (Maybe VerticalAttached)
  , _buttonsConfig_width :: Active t (Maybe Width)
  , _buttonsConfig_floated :: Active t (Maybe Floated)

  , _buttonsConfig_elConfig :: ActiveElConfig t
  }
#ifdef USE_TEMPLATE_HASKELL
makeLensesWith (lensRules & simpleLenses .~ True) ''ButtonsConfig
#endif

instance HasElConfig t (ButtonsConfig t) where
  elConfig = buttonsConfig_elConfig

instance Reflex t => Default (ButtonsConfig t) where
  def = ButtonsConfig
    { _buttonsConfig_basic = pure False
    , _buttonsConfig_icon = pure False
    , _buttonsConfig_labeledIcon = pure False
    , _buttonsConfig_vertical = pure False
    , _buttonsConfig_compact = pure False

    , _buttonsConfig_color = pure Nothing
    , _buttonsConfig_size = pure Nothing
    , _buttonsConfig_attached = pure Nothing
    , _buttonsConfig_width = pure Nothing
    , _buttonsConfig_floated = pure Nothing

    , _buttonsConfig_elConfig = def
    }

buttonsConfigClasses :: Reflex t
  => ButtonsConfig t -> Active t Classes
buttonsConfigClasses ButtonsConfig {..} = dynClasses
  [ pure $ Just "ui buttons"

  , boolClass "basic" _buttonsConfig_basic
  , boolClass "icon" _buttonsConfig_icon
  , boolClass "labeled icon" _buttonsConfig_labeledIcon
  , boolClass "vertical" _buttonsConfig_vertical
  , boolClass "compact" _buttonsConfig_compact

  , fmap toClassText <$> _buttonsConfig_color
  , fmap toClassText <$> _buttonsConfig_size
  , fmap toClassText <$> _buttonsConfig_attached
  , fmap toClassText <$> _buttonsConfig_width
  , fmap toClassText <$> _buttonsConfig_floated
  ]

data LabeledButtonConfig t = LabeledButtonConfig
  { _labeledButtonConfig_side :: Active t Labeled
  , _labeledButtonConfig_elConfig :: ActiveElConfig t
  }
#ifdef USE_TEMPLATE_HASKELL
makeLensesWith (lensRules & simpleLenses .~ True) ''LabeledButtonConfig
#endif

instance HasElConfig t (LabeledButtonConfig t) where
  elConfig = labeledButtonConfig_elConfig

instance Reflex t
  => Default (LabeledButtonConfig t) where
  def = LabeledButtonConfig
    { _labeledButtonConfig_side = pure RightLabeled
    , _labeledButtonConfig_elConfig = def
    }

labeledButtonConfigClasses :: Reflex t
  => LabeledButtonConfig t -> Active t Classes
labeledButtonConfigClasses LabeledButtonConfig {..} = dynClasses
  [ pure $ Just "ui button"
  , Just . toClassText <$> _labeledButtonConfig_side
  ]

data AnimatedButtonType = Animated | VerticalAnimated | AnimatedFade
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToClassText AnimatedButtonType where
  toClassText Animated = "animated"
  toClassText VerticalAnimated = "vertical animated"
  toClassText AnimatedFade = "animated fade"

data AnimatedButton t m = AnimatedButton
  { _animatedButton_type :: Active t AnimatedButtonType
  , _animatedButton_hiddenContent :: m ()
  }
#ifdef USE_TEMPLATE_HASKELL
makeLensesWith (lensRules & simpleLenses .~ True) ''AnimatedButton
#endif


instance (Reflex t, Applicative m)
  => Default (AnimatedButton t m) where
  def = AnimatedButton
    { _animatedButton_type = pure Animated
    , _animatedButton_hiddenContent = pure ()
    }

-- | HTML tags that can be used for buttons
data ButtonType
  = ButtonButton
  | ResetButton
  | SubmitButton
  | LinkButton
  | DivButton

toTagText :: ButtonType -> Text
toTagText = \case
  SubmitButton -> "button"
  ButtonButton -> "button"
  ResetButton -> "button"
  LinkButton -> "a"
  DivButton -> "div"

data ButtonConfig t m = ButtonConfig
  { _buttonConfig_disabled     :: Active t Bool
  , _buttonConfig_compact      :: Active t Bool
  , _buttonConfig_basic        :: Active t Bool
  , _buttonConfig_icon         :: Active t Bool
  , _buttonConfig_inverted     :: Active t Bool
  , _buttonConfig_loading      :: Active t Bool
  , _buttonConfig_fluid        :: Active t Bool
  , _buttonConfig_circular     :: Active t Bool

  , _buttonConfig_color        :: Active t (Maybe Color)
  , _buttonConfig_size         :: Active t (Maybe Size)
  , _buttonConfig_emphasis     :: Active t (Maybe Emphasis)
  , _buttonConfig_positive     :: Active t (Maybe Positive)
  , _buttonConfig_social       :: Active t (Maybe Social)
  , _buttonConfig_floated      :: Active t (Maybe Floated)
  , _buttonConfig_labeledIcon  :: Active t (Maybe Labeled)
  , _buttonConfig_attached     :: Active t (Maybe ExclusiveAttached)

  , _buttonConfig_animated     :: Maybe (AnimatedButton t m)
  , _buttonConfig_type         :: ButtonType
  , _buttonConfig_elConfig     :: ActiveElConfig t
  }
#ifdef USE_TEMPLATE_HASKELL
makeLensesWith (lensRules & simpleLenses .~ True) ''ButtonConfig
#endif

instance HasElConfig t (ButtonConfig t m) where
  elConfig = buttonConfig_elConfig

instance Reflex t => Default (ButtonConfig t m) where
  def = ButtonConfig
    { _buttonConfig_disabled = pure False
    , _buttonConfig_compact = pure False
    , _buttonConfig_basic = pure False
    , _buttonConfig_icon = pure False
    , _buttonConfig_inverted = pure False
    , _buttonConfig_loading = pure False
    , _buttonConfig_fluid = pure False
    , _buttonConfig_circular = pure False

    , _buttonConfig_color = pure Nothing
    , _buttonConfig_size = pure Nothing
    , _buttonConfig_emphasis = pure Nothing
    , _buttonConfig_positive = pure Nothing
    , _buttonConfig_social = pure Nothing
    , _buttonConfig_floated = pure Nothing
    , _buttonConfig_labeledIcon = pure Nothing
    , _buttonConfig_attached = pure Nothing

    , _buttonConfig_animated = Nothing
    , _buttonConfig_type = ButtonButton
    , _buttonConfig_elConfig = def
    }

-- | Change success to positive and error to negative, since button does not
-- support success/error types
filterPositive :: Positive -> Positive
filterPositive Positive = Positive
filterPositive Negative = Negative
filterPositive Success = Positive
filterPositive Error = Negative

buttonConfigClasses :: Reflex t
  => ButtonConfig t m -> Active t Classes
buttonConfigClasses ButtonConfig {..} = dynClasses
  [ pure $ Just "ui button"
  , boolClass "disabled" _buttonConfig_disabled
  , boolClass "compact" _buttonConfig_compact
  , boolClass "basic" _buttonConfig_basic
  , boolClass "icon" _buttonConfig_icon
  , boolClass "inverted" _buttonConfig_inverted
  , boolClass "loading" _buttonConfig_loading
  , boolClass "fluid" _buttonConfig_fluid
  , boolClass "circular" _buttonConfig_circular
  , fmap ((<> " icon") . toClassText) <$> _buttonConfig_labeledIcon
  , fmap toClassText <$> _buttonConfig_color
  , fmap toClassText <$> _buttonConfig_size
  , fmap toClassText <$> _buttonConfig_emphasis
  , fmap (toClassText . filterPositive) <$> _buttonConfig_positive
  , fmap toClassText <$> _buttonConfig_social
  , fmap toClassText <$> _buttonConfig_floated
  , fmap toClassText <$> _buttonConfig_attached
  , traverse (fmap toClassText . _animatedButton_type) _buttonConfig_animated
  ]


buttons'
  :: UI t m => ButtonsConfig t -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
buttons' config@ButtonsConfig {..} = ui' "div" elConf
  where
   elConf = _buttonsConfig_elConfig <> def
      { _classes = buttonsConfigClasses config }

buttons :: UI t m => ButtonsConfig t -> m a -> m a
buttons c = fmap snd . buttons' c

conditionalWithText'
  :: UI t m => Active t Text
  -> m (Element EventResult (DomBuilderSpace m) t)
conditionalWithText' dataText
  = fst <$> ui' "div" config blank
  where
    config = def
      & classes |~ "or"
      & attrs .~ fmap ("data-text" =:) dataText

conditionalWithText :: UI t m => Active t Text -> m ()
conditionalWithText = void . conditionalWithText'

conditional' :: UI t m => m (Element EventResult (DomBuilderSpace m) t)
conditional' = fst <$> divClass' "or" blank

conditional :: UI t m => m ()
conditional = void conditional'

button'
  :: UI t m => ButtonConfig t m -> m ()
  -> m (Element EventResult (DomBuilderSpace m) t, Event t ())
button' config@ButtonConfig {..} content = do
  (e, _) <- ui' (toTagText _buttonConfig_type) elConf $
    case _buttonConfig_animated of
      Just (AnimatedButton _ hiddenContent) -> do
        divClass "visible content" content
        divClass "hidden content" hiddenContent
      Nothing -> content
  pure (e, domEvent Click e)
  where
    elConf = _buttonConfig_elConfig <> def
      { _classes = buttonConfigClasses config
      , _attrs = pure $ case _buttonConfig_type of
        SubmitButton -> "type" =: "submit"
        ButtonButton -> "type" =: "button"
        ResetButton  -> "type" =: "reset"
        _ -> mempty
      }

button :: UI t m => ButtonConfig t m -> m () -> m (Event t ())
button c = fmap snd . button' c

labeledButton'
  :: UI t m => LabeledButtonConfig t -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, Event t ())
labeledButton' config@LabeledButtonConfig{..} content = do
  (e, _) <- ui' "div" elConf content
  pure (e, domEvent Click e)
  where
    elConf = _labeledButtonConfig_elConfig <> def
      { _classes = labeledButtonConfigClasses config }

labeledButton :: UI t m => LabeledButtonConfig t -> m a -> m (Event t ())
labeledButton c = fmap snd . labeledButton' c

#ifndef USE_TEMPLATE_HASKELL
#include "Button.th.hs"
#endif
