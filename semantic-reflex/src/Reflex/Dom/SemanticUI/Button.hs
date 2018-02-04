{-# LANGUAGE TemplateHaskell #-}

module Reflex.Dom.SemanticUI.Button
  (

  -- * Normal buttons
    button, button'
  , ButtonType (..)
  , ButtonConfig (..)
  , buttonColor
  , buttonSize
  , buttonEmphasis
  , buttonPositive
  , buttonSocial
  , buttonFloated
  , buttonDisabled
  , buttonCompact
  , buttonBasic
  , buttonIcon
  , buttonInverted
  , buttonLoading
  , buttonFluid
  , buttonCircular
  , buttonLabeledIcon
  , buttonAttached
  , buttonAnimated
  , buttonType
  , buttonElConfig

  -- * Labeled buttons
  , labeledButton, labeledButton'
  , LabeledButtonConfig (..)
  , labeledButtonSide
  , labeledButtonElConfig

  -- * Conditionals
  , conditional, conditional'
  , conditionalWithText, conditionalWithText'

  -- * Animated button config
  , AnimatedButtonType (..)
  , AnimatedButton (..)
  , animatedButtonHiddenContent
  , animatedButtonType

  -- * Groups of buttons
  , buttons, buttons'
  , ButtonsConfig (..)
  , buttonsColor
  , buttonsSize
  , buttonsBasic
  , buttonsIcon
  , buttonsLabeledIcon
  , buttonsVertical
  , buttonsCompact
  , buttonsAttached
  , buttonsWidth
  , buttonsFloated
  , buttonsElConfig

  ) where

import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
import Control.Monad (void)
import Data.Default
import Data.Semigroup hiding (First)
import Data.Text (Text)
import Reflex
import Reflex.Dom.Core hiding (Error, button)

import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data ButtonsConfig t = ButtonsConfig
  { _buttonsBasic :: Dynamic t Bool
  , _buttonsIcon :: Dynamic t Bool
  , _buttonsLabeledIcon :: Dynamic t Bool
  , _buttonsVertical :: Dynamic t Bool
  , _buttonsCompact :: Dynamic t Bool

  , _buttonsColor :: Dynamic t (Maybe Color)
  , _buttonsSize :: Dynamic t (Maybe Size)
  , _buttonsAttached :: Dynamic t (Maybe VerticalAttached)
  , _buttonsWidth :: Dynamic t (Maybe Width)
  , _buttonsFloated :: Dynamic t (Maybe Floated)

  , _buttonsElConfig :: ActiveElConfig t
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''ButtonsConfig

instance HasElConfig t (ButtonsConfig t) where
  elConfig = buttonsElConfig

instance Reflex t => Default (ButtonsConfig t) where
  def = ButtonsConfig
    { _buttonsBasic = pure False
    , _buttonsIcon = pure False
    , _buttonsLabeledIcon = pure False
    , _buttonsVertical = pure False
    , _buttonsCompact = pure False

    , _buttonsColor = pure Nothing
    , _buttonsSize = pure Nothing
    , _buttonsAttached = pure Nothing
    , _buttonsWidth = pure Nothing
    , _buttonsFloated = pure Nothing

    , _buttonsElConfig = def
    }

buttonsConfigClasses :: Reflex t
  => ButtonsConfig t -> Dynamic t Classes
buttonsConfigClasses ButtonsConfig {..} = dynClasses
  [ pure $ Just "ui buttons"

  , boolClass "basic" _buttonsBasic
  , boolClass "icon" _buttonsIcon
  , boolClass "labeled icon" _buttonsLabeledIcon
  , boolClass "vertical" _buttonsVertical
  , boolClass "compact" _buttonsCompact

  , fmap toClassText <$> _buttonsColor
  , fmap toClassText <$> _buttonsSize
  , fmap toClassText <$> _buttonsAttached
  , fmap toClassText <$> _buttonsWidth
  , fmap toClassText <$> _buttonsFloated
  ]

data LabeledButtonConfig t = LabeledButtonConfig
  { _labeledButtonSide :: Dynamic t Labeled
  , _labeledButtonElConfig :: ActiveElConfig t
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''LabeledButtonConfig

instance HasElConfig t (LabeledButtonConfig t) where
  elConfig = labeledButtonElConfig

instance Reflex t
  => Default (LabeledButtonConfig t) where
  def = LabeledButtonConfig
    { _labeledButtonSide = pure RightLabeled
    , _labeledButtonElConfig = def
    }

labeledButtonConfigClasses :: Reflex t
  => LabeledButtonConfig t -> Dynamic t Classes
labeledButtonConfigClasses LabeledButtonConfig {..} = dynClasses
  [ pure $ Just "ui button"
  , Just . toClassText <$> _labeledButtonSide
  ]

data AnimatedButtonType = Animated | VerticalAnimated | AnimatedFade
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToClassText AnimatedButtonType where
  toClassText Animated = "animated"
  toClassText VerticalAnimated = "vertical animated"
  toClassText AnimatedFade = "animated fade"

data AnimatedButton t m = AnimatedButton
  { _animatedButtonType :: Dynamic t AnimatedButtonType
  , _animatedButtonHiddenContent :: m ()
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''AnimatedButton

instance (Reflex t, Applicative m)
  => Default (AnimatedButton t m) where
  def = AnimatedButton
    { _animatedButtonType = pure Animated
    , _animatedButtonHiddenContent = pure ()
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
  { _buttonDisabled     :: Dynamic t Bool
  , _buttonCompact      :: Dynamic t Bool
  , _buttonBasic        :: Dynamic t Bool
  , _buttonIcon         :: Dynamic t Bool
  , _buttonInverted     :: Dynamic t Bool
  , _buttonLoading      :: Dynamic t Bool
  , _buttonFluid        :: Dynamic t Bool
  , _buttonCircular     :: Dynamic t Bool

  , _buttonColor        :: Dynamic t (Maybe Color)
  , _buttonSize         :: Dynamic t (Maybe Size)
  , _buttonEmphasis     :: Dynamic t (Maybe Emphasis)
  , _buttonPositive     :: Dynamic t (Maybe Positive)
  , _buttonSocial       :: Dynamic t (Maybe Social)
  , _buttonFloated      :: Dynamic t (Maybe Floated)
  , _buttonLabeledIcon  :: Dynamic t (Maybe Labeled)
  , _buttonAttached     :: Dynamic t (Maybe ExclusiveAttached)

  , _buttonAnimated     :: Maybe (AnimatedButton t m)
  , _buttonType         :: ButtonType
  , _buttonElConfig     :: ActiveElConfig t
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''ButtonConfig

instance HasElConfig t (ButtonConfig t m) where
  elConfig = buttonElConfig

instance Reflex t => Default (ButtonConfig t m) where
  def = ButtonConfig
    { _buttonDisabled = pure False
    , _buttonCompact = pure False
    , _buttonBasic = pure False
    , _buttonIcon = pure False
    , _buttonInverted = pure False
    , _buttonLoading = pure False
    , _buttonFluid = pure False
    , _buttonCircular = pure False

    , _buttonColor = pure Nothing
    , _buttonSize = pure Nothing
    , _buttonEmphasis = pure Nothing
    , _buttonPositive = pure Nothing
    , _buttonSocial = pure Nothing
    , _buttonFloated = pure Nothing
    , _buttonLabeledIcon = pure Nothing
    , _buttonAttached = pure Nothing

    , _buttonAnimated = Nothing
    , _buttonType = ButtonButton
    , _buttonElConfig = def
    }

-- | Change success to positive and error to negative, since button does not
-- support success/error types
filterPositive :: Positive -> Positive
filterPositive Positive = Positive
filterPositive Negative = Negative
filterPositive Success = Positive
filterPositive Error = Negative

buttonConfigClasses :: Reflex t
  => ButtonConfig t m -> Dynamic t Classes
buttonConfigClasses ButtonConfig {..} = dynClasses
  [ pure $ Just "ui button"
  , boolClass "disabled" _buttonDisabled
  , boolClass "compact" _buttonCompact
  , boolClass "basic" _buttonBasic
  , boolClass "icon" _buttonIcon
  , boolClass "inverted" _buttonInverted
  , boolClass "loading" _buttonLoading
  , boolClass "fluid" _buttonFluid
  , boolClass "circular" _buttonCircular
  , fmap ((<> " icon") . toClassText) <$> _buttonLabeledIcon
  , fmap toClassText <$> _buttonColor
  , fmap toClassText <$> _buttonSize
  , fmap toClassText <$> _buttonEmphasis
  , fmap (toClassText . filterPositive) <$> _buttonPositive
  , fmap toClassText <$> _buttonSocial
  , fmap toClassText <$> _buttonFloated
  , fmap toClassText <$> _buttonAttached
  , traverse (fmap toClassText . _animatedButtonType) _buttonAnimated
  ]


buttons'
  :: MonadWidget t m
  => ButtonsConfig t -> m a -> m (El t, a)
buttons' config@ButtonsConfig {..} = uiElement' "div" elConf
  where
   elConf = _buttonsElConfig <> def
      { _classes = buttonsConfigClasses config }

buttons
  :: MonadWidget t m
  => ButtonsConfig t -> m a -> m a
buttons c = fmap snd . buttons' c

conditionalWithText'
  :: MonadWidget t m => Dynamic t Text -> m (El t)
conditionalWithText' dataText
  = fst <$> uiElement' "div" config blank
  where
    config = def
      & classes |~ "or"
      & attrs .~ fmap ("data-text" =:) dataText

conditionalWithText
  :: MonadWidget t m => Dynamic t Text -> m ()
conditionalWithText = void . conditionalWithText'

conditional'
  :: MonadWidget t m => m (El t)
conditional' = fst <$> divClass' "or" blank

conditional :: MonadWidget t m => m ()
conditional = void conditional'

button'
  :: MonadWidget t m
  => ButtonConfig t m -> m () -> m (El t, Event t ())
button' config@ButtonConfig {..} content = do
  (e, _) <- uiElement' (toTagText _buttonType) elConf $
    case _buttonAnimated of
      Just (AnimatedButton _ hiddenContent) -> do
        divClass "visible content" content
        divClass "hidden content" hiddenContent
      Nothing -> content
  pure (e, domEvent Click e)
  where
    elConf = _buttonElConfig <> def
      { _classes = buttonConfigClasses config
      , _attrs = pure $ case _buttonType of
        SubmitButton -> "type" =: "submit"
        ButtonButton -> "type" =: "button"
        ResetButton  -> "type" =: "reset"
        _ -> mempty
      }

button
  :: MonadWidget t m
  => ButtonConfig t m -> m () -> m (Event t ())
button c = fmap snd . button' c

labeledButton'
  :: MonadWidget t m
  => LabeledButtonConfig t -> m a -> m (El t, Event t ())
labeledButton' config@LabeledButtonConfig{..} content = do
  (e, _) <- uiElement' "div" elConf content
  pure (e, domEvent Click e)
  where
    elConf = _labeledButtonElConfig <> def
      { _classes = labeledButtonConfigClasses config }

labeledButton
  :: MonadWidget t m
  => LabeledButtonConfig t -> m a -> m (Event t ())
labeledButton c = fmap snd . labeledButton' c

