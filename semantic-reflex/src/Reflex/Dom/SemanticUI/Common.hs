{-# LANGUAGE ConstraintKinds          #-}
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
{-# LANGUAGE TypeFamilyDependencies   #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving     #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveAnyClass     #-}

module Reflex.Dom.SemanticUI.Common where

------------------------------------------------------------------------------
import           Control.Lens ((^.), set, ASetter)
import           Control.Monad (void)
import Data.Default
import Data.String
import Data.Semigroup
import Data.Map (Map)
import           Data.Text (Text)
import qualified Data.Text as T
import           Language.Javascript.JSaddle
import           Reflex.Dom.Core
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- JSaddle helpers

-- | Javascript console.log
consoleLog :: ToJSVal a => a -> JSM ()
consoleLog a = do
  console <- jsg ("console" :: Text)
  void $ console ^. js1 ("log" :: Text) a

consoleTime :: JSString -> JSM ()
consoleTime a = do
  console <- jsg ("console" :: Text)
  void $ console ^. js1 ("time" :: Text) a

consoleTimeEnd :: JSString -> JSM ()
consoleTimeEnd a = do
  console <- jsg ("console" :: Text)
  void $ console ^. js1 ("timeEnd" :: Text) a

-- | Catch any JSExceptions and log them to the console. Useful for debugging
-- ghc implementations, especially wth jsaddle-warp.
catchJS :: JSM () -> JSM ()
catchJS action = catch action handle
  where handle (JSException e) = consoleLog e

-- | The jQuery function, often used by the alias $(..) in javascript.
jQuery :: ToJSVal a => a -> JSM JSVal
jQuery = jsg1 ("jQuery" :: Text)

------------------------------------------------------------------------------
-- | Temporary...will be moved out of here eventually.
tshow :: Show a => a -> Text
tshow = T.pack . show

-- | Map with index
imap :: (Int -> a -> b) -> [a] -> [b]
imap f = go 0
  where
    go _ [] = []
    go i (x:xs) = f i x : go (succ i) xs

-- | Safe indexing into lists
(!?) :: [a] -> Int -> Maybe a
[] !? _ = Nothing
(x:_) !? 0 = Just x
(_:xs) !? n = xs !? (n - 1)

------------------------------------------------------------------------------

newtype ClassText = ClassText (Maybe Text) deriving (Eq, Show)

getClass :: ClassText -> Text
getClass (ClassText (Just a)) = a
getClass (ClassText Nothing) = ""

instance IsString ClassText where
  fromString str = ClassText $ Just $ fromString str

instance Monoid ClassText where
  mappend = (<>)
  mempty = ClassText Nothing

instance Semigroup ClassText where
  ClassText (Just a) <> ClassText (Just b) = ClassText $ Just $ a <> " " <> b
  ClassText ma <> ClassText mb = ClassText $ ma <> mb

memptyUnless :: Monoid m => m -> Bool -> m
memptyUnless _ False = mempty
memptyUnless m True = m

class ToClassText a where
  toClassText :: a -> ClassText

instance ToClassText a => ToClassText (Maybe a) where
  toClassText Nothing = mempty
  toClassText (Just a) = toClassText a

nothingIf :: Eq a => a -> Maybe a -> Maybe a
nothingIf x (Just y) | x == y = Nothing
nothingIf _ m = m

justWhen :: Bool -> a -> Maybe a
justWhen True = Just
justWhen False = const Nothing

data Active t a
  = Static !a
  | Dynamic !(Dynamic t a)

active :: (a -> b) -> (Dynamic t a -> b) -> Active t a -> b
active f _ (Static a) = f a
active _ f (Dynamic a) = f a

instance Reflex t => Functor (Active t) where
  fmap f (Static a) = Static (f a)
  fmap f (Dynamic a) = Dynamic (fmap f a)

instance Reflex t => Applicative (Active t) where
  pure a = Static a
  Static f <*> Static a = Static (f a)
  Dynamic f <*> Dynamic a = Dynamic (f <*> a)
  Static f <*> Dynamic a = Dynamic (f <$> a)
  Dynamic f <*> Static a = Dynamic (($ a) <$> f)

instance IsString a => IsString (Active t a) where
  fromString = Static . fromString

instance (Reflex t, Semigroup a) => Semigroup (Active t a) where
  Static a <> Static b = Static (a <> b)
  Static a <> Dynamic b = Dynamic ((a <>) <$> b)
  Dynamic a <> Static b = Dynamic ((<> b) <$> a)
  Dynamic a <> Dynamic b = Dynamic (zipDynWith (<>) a b)

instance (Reflex t, Monoid a, Semigroup a) => Monoid (Active t a) where
  mempty = Static mempty
  mappend = (<>)

elActiveAttr'
  :: (PostBuild t m, DomBuilder t m)
  => Text -> Active t (Map Text Text) -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
elActiveAttr' elType (Static attrs) = elAttr' elType attrs
elActiveAttr' elType (Dynamic attrs) = elDynAttr' elType attrs


elActiveAttr
  :: (PostBuild t m, DomBuilder t m)
  => Text -> Active t (Map Text Text) -> m a -> m a
elActiveAttr t a = fmap snd . elActiveAttr' t a

activeText :: (PostBuild t m, DomBuilder t m) => Active t Text -> m ()
activeText (Static t) = text t
activeText (Dynamic t) = dynText t

runActive :: (MonadWidget t m, UI t m a) => Active t a -> m ()
runActive (Dynamic a) = void $ dyn $ ui_ <$> a
runActive (Static a) = ui_ a



instance Reflex t => IsString (Dynamic t Text) where
  fromString = pure . fromString

class UI t m a where
  type Return t m a
  ui' :: MonadWidget t m => a -> m (El t, Return t m a)
--  uiDyn :: MonadWidget t m => Dynamic t a -> m (El t, Return t m a)

ui :: (MonadWidget t m, UI t m a) => a -> m (Return t m a)
ui = fmap snd . ui'

ui_ :: (MonadWidget t m, UI t m a) => a -> m ()
ui_ = void . ui

part_ :: (MonadWidget t m, Part t m a) => a -> m ()
part_ = void . part

part :: (MonadWidget t m, Part t m a) => a -> m (Return t m a)
part = fmap snd . part'

class ToItem a where
  toItem :: a -> a

class (ToPart a, UI t m a) => Part t m a where
  part' :: MonadWidget t m => a -> m (El t, Return t m a)
  part' = ui' . toPart

instance (ToPart a, UI t m a) => Part t m a where

instance UI t m Text where
  type Return t m Text = ()
  ui' = el' "" . text

class ToPart a where
  toPart :: a -> a

---------

data RenderWhen t a
  = NeverRender
  | AlwaysRender a
  | RenderWhen (Dynamic t Bool) a

instance Reflex t => Default (RenderWhen t a) where
  def = NeverRender

runRenderWhen
  :: forall t m a. (UI t m a, MonadWidget t m)
  => (a -> m (El t, Return t m a))
  -> RenderWhen t a
  -> m (Dynamic t (Maybe (El t, Return t m a)))
runRenderWhen _ NeverRender = return $ pure Nothing
runRenderWhen render (AlwaysRender widget) = fmap (pure . Just) $ render widget
runRenderWhen render (RenderWhen when widget) = do

  trimmed :: Dynamic t Bool <- holdUniqDyn when

  res <- dyn $ fmap (sequence . f) trimmed
  holdDyn Nothing res

  where
    f :: Bool -> Maybe (m (El t, Return t m a))
    f False = Nothing
    f True = Just $ render widget

---------

(|~) :: (Applicative (f t'), Reflex t') => ASetter s t a (f t' b) -> b -> s -> t
l |~ b = set l (pure b)

(|?~) :: (Applicative (f t'), Reflex t') => ASetter s t a (f t' (Maybe b)) -> b -> s -> t
l |?~ b = set l (pure $ Just b)


data Floated = LeftFloated | RightFloated deriving (Eq, Show)

instance ToClassText Floated where
  toClassText LeftFloated = "left floated"
  toClassText RightFloated = "right floated"

data Size = Mini | Tiny | Small | Medium | Large | Big | Huge | Massive deriving (Eq, Show)

instance ToClassText Size where
  toClassText Mini = "mini"
  toClassText Tiny = "tiny"
  toClassText Small = "small"
  toClassText Medium = "medium"
  toClassText Large = "large"
  toClassText Big = "big"
  toClassText Huge = "huge"
  toClassText Massive = "massive"

data HorizontalAttached = LeftAttached | RightAttached deriving (Eq, Show)
data VerticalAttached = TopAttached | BottomAttached deriving (Eq, Show)

data ExclusiveAttached
  = Horizontally HorizontalAttached
  | Vertically VerticalAttached
  deriving (Eq, Show)

instance ToClassText ExclusiveAttached where
  toClassText (Horizontally h) = "attached" <> toClassText h
  toClassText (Vertically v) = "attached" <> toClassText v

instance ToClassText VerticalAttached where
  toClassText TopAttached = "top"
  toClassText BottomAttached = "bottom"

instance ToClassText HorizontalAttached where
  toClassText LeftAttached = "left"
  toClassText RightAttached = "right"

combineAttached :: Maybe VerticalAttached -> Maybe HorizontalAttached -> ClassText
combineAttached Nothing Nothing = mempty
combineAttached mv mh = mconcat [ toClassText mv, toClassText mh, "attached" ]

data Color
  = Red
  | Orange
  | Yellow
  | Olive
  | Green
  | Teal
  | Blue
  | Violet
  | Purple
  | Pink
  | Brown
  | Grey
  | Black
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance ToClassText Color where
  toClassText Red = "red"
  toClassText Orange = "orange"
  toClassText Yellow = "yellow"
  toClassText Olive = "olive"
  toClassText Green = "green"
  toClassText Teal = "teal"
  toClassText Blue = "blue"
  toClassText Violet = "violet"
  toClassText Purple = "purple"
  toClassText Pink = "pink"
  toClassText Brown = "brown"
  toClassText Grey = "grey"
  toClassText Black = "black"
