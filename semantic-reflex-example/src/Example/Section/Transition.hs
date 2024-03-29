{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}

module Example.Section.Transition where

import Control.Lens
import Control.Monad ((<=<))
import Data.Char (isUpper)
import Data.Foldable (traverse_, for_)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.SemanticUI
import Reflex.Dom.Core (text)

import Example.QQ
import Example.Common

spaceBeforeUpper :: String -> String
spaceBeforeUpper "" = ""
spaceBeforeUpper (c:cs)
  | isUpper c = ' ' : c : spaceBeforeUpper cs
  | otherwise = c : spaceBeforeUpper cs

animalUrl :: Reflex t => Text -> Active t Text
animalUrl a = pure $ "images/animals/" <> a <> ".png"

mkButton :: (MonadWidget t m, Prerender t m) => TransitionType
         -> m (Event t TransitionType)
mkButton t = do
  run <- button (def & buttonConfig_fluid |~ True) $ do
    text $ T.pack $ spaceBeforeUpper $ show t
  return $ t <$ run

mkImages :: (MonadWidget t m, Prerender t m) => Text -> Event t TransitionType -> m ()
mkImages animal evt = do
  divider $ def & dividerConfig_hidden |~ True
  let imgConfig =  def
        & imageConfig_size |?~ Small & imageConfig_shape |?~ Rounded & imageConfig_inline |~ True
  image (imgConfig & imageConfig_spaced |?~ RightSpaced
                   & action ?~ def { _action_transition = ffor evt $ \t -> Transition t Nothing def }) $
                     Left $ Img (animalUrl animal) def
  image (imgConfig & imageConfig_spaced |?~ LeftSpaced
                   & action ?~ def { _action_transition = ffor evt $ \t -> Transition t Nothing (def { _transitionConfig_cancelling = True }) }) $
                     Left $ Img (animalUrl animal) def

mkButtons :: (MonadWidget t m, Prerender t m) => Text -> [TransitionType] -> m ()
mkButtons _ [] = return ()
mkButtons animal (transType:[]) = do
  evt <- mkButton transType
  mkImages animal evt
mkButtons animal types = do
  evt <- buttons buttonsConfig $ leftmost <$> traverse mkButton types
  mkImages animal evt
  where buttonsConfig = def
          & buttonsConfig_width |?~ toEnum (length types)
          & buttonsConfig_size |?~ Small

transitions :: (MonadWidget t m, Prerender t m) => Section t m
transitions = Section "Transition" (simpleLink "https://semantic-ui.com/modules/transition.html") $ do

  message (def & messageConfig_type |?~ InfoMessage) $ paragraph $ do
    icon "announcement" def
    text "The implementation of the Transition module does not depend on the Semantic UI or jQuery Javascript libraries."

  paragraph $ text "Any of the components in this library are capable of Semantic UI transitions. The difference between animations and transitions is more clear cut, with different data constructors for each:"

  hscode $(printDefinition id stripParens ''Action)

  hscode $(printDefinition oneline stripParens ''Transition)
  hscode $(printDefinition oneline stripParens ''TransitionType)
  hscode $(printDefinition id stripParens ''TransitionConfig)

  hscode $(printDefinition oneline stripParens ''Animation)
  hscode $(printDefinition oneline stripParens ''AnimationType)
  hscode $(printDefinition id stripParens ''AnimationConfig)

  paragraph $ text "If the direction of a transition event is not specified, the transition will flip the current state of the element. Animation events will cause hidden elements to be shown, and they will remain shown after the animation finishes."

  paragraph $ text "Animation or Transition events are placed into a queue which allows time for each transition to finish. The animation or transition speeds are specified in seconds using the duration config fields."

  pageHeader H3 def $ text "Examples"

  mkExample "Animations" (def
    & subtitle ?~ text "An element can be animated to draw attention to it")
    [example|
  divClass "ui seven column vertically padded grid" $
    for_ (zip [Jiggle .. Glow] ["duck", "cat", "eagle", "koala", "tiger", "kangaroo", "lion"]) $
      \(anim, animal) -> divClass "center aligned column" $ do
        run <- button (def & style |~ Style "margin-bottom: 1em") $
          text $ tshow anim
        image (def & imageConfig_size |?~ Small & imageConfig_shape |?~ Rounded
                   & action ?~ def { _action_animation = Animation anim def <$ run }) $
          Left $ Img (animalUrl animal) def
  |]

  mkExample "Cancelling Animations" (def
    & subtitle ?~ text "Transitions can cancel override queued transitions")
    [example|
  divClass "ui seven column vertically padded grid" $
    for_ (zip [Jiggle .. Glow] ["duck", "cat", "eagle", "koala", "tiger", "kangaroo", "lion"]) $
      \(anim, animal) -> divClass "center aligned column" $ do
        run <- button (def & style |~ Style "margin-bottom: 1em") $
          text $ tshow anim
        image (def & imageConfig_size |?~ Small & imageConfig_shape |?~ Rounded
                   & action ?~ def { _action_animation = Animation anim (def
                      { _animationConfig_cancelling = True }) <$ run }) $
          Left $ Img (animalUrl animal) def
  |]

  mkExample "Transitions" (def
    & subtitle ?~ text "An element can be hidden or shown using a transition")
    [example|
  evt <- buttons def $ do
    tog <- button def $ text "Toggle"
    show <- button def $ text "Show"
    hide <- button def $ text "Hide"
    pure $ leftmost
      [ Nothing <$ tog
      , Just In <$ show
      , Just Out <$ hide
      ]

  divider $ def & dividerConfig_hidden |~ True

  image (def
    & imageConfig_size |?~ Small & imageConfig_shape |?~ Rounded
    & action ?~ def { _action_transition = ffor evt $ \d -> Transition Instant d def }) $
    Left $ Img (animalUrl "crocodile") def
  |]

  mkExample "Scale" (def
    & subtitle ?~ text "An element can scale in or out")
    [example|
  mkButtons "gorilla" [Scale]
  |]

  mkExample "Fade" (def
    & subtitle ?~ text "An element can fade in or out")
    [example|
  mkButtons "cow" [Fade, FadeLeft, FadeRight, FadeUp, FadeDown]
  |]

  mkExample "Flip" (def
    & subtitle ?~ text "An element can flip in or out")
    [example|
  mkButtons "panda" [HorizontalFlip, VerticalFlip]
  |]

  mkExample "Drop" (def
    & subtitle ?~ text "An element can drop in or out")
    [example|
  mkButtons "turtle" [Drop]
  |]

  mkExample "Fly" (def
    & subtitle ?~ text "An element can fly in or out")
    [example|
  mkButtons "bird" [FlyLeft, FlyRight, FlyUp, FlyDown]
  |]

  mkExample "Swing" (def
    & subtitle ?~ text "An element can swing in or out")
    [example|
  mkButtons "monkey" [SwingLeft, SwingRight, SwingUp, SwingDown]
  |]

  mkExample "Browse" (def
    & subtitle ?~ text "An element can browse in or out")
    [example|
  mkButtons "frog" [Browse, BrowseRight]
  |]

  mkExample "Slide" (def
    & subtitle ?~ text "An element can slide in or out")
    [example|
  mkButtons "wolf" [SlideDown, SlideUp, SlideLeft, SlideRight]
  |]

  mkExample "Zoom" (def
    & subtitle ?~ text "An element can zoom into view from far away")
    [example|
  mkButtons "bat" [Zoom]
  |]

  return ()
