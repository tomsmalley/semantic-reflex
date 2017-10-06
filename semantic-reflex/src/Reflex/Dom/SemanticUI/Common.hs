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
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE LambdaCase     #-}

module Reflex.Dom.SemanticUI.Common where

------------------------------------------------------------------------------
import Control.Monad.Fix (MonadFix)
import Control.Lens ((^.), set, ASetter)
import Control.Monad (void, (<=<))
import Data.Default
import Data.Kind (Type)
import Data.String
import Data.Semigroup
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Language.Javascript.JSaddle
import Reflex.Dom.Core hiding (Link)
import Data.Foldable (fold)
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
  toClassText :: a -> Text

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
  fmap f (Static a)  = Static  $ f a
  fmap f (Dynamic a) = Dynamic $ fmap f a

instance Reflex t => Applicative (Active t) where
  pure a = Static a
  Static f  <*> Static a  = Static  $ f a
  Static f  <*> Dynamic a = Dynamic $ f <$> a
  Dynamic f <*> Static a  = Dynamic $ fmap ($ a) f
  Dynamic f <*> Dynamic a = Dynamic $ f <*> a

instance IsString a => IsString (Active t a) where
  fromString = Static . fromString

instance (Reflex t, Semigroup a) => Semigroup (Active t a) where
  Static a  <> Static b  = Static  $ a <> b
  Static a  <> Dynamic b = Dynamic $ fmap (a <>) b
  Dynamic a <> Static b  = Dynamic $ fmap (<> b) a
  Dynamic a <> Dynamic b = Dynamic $ zipDynWith (<>) a b

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

activeMaybe :: (PostBuild t m, DomBuilder t m) => (a -> m ()) -> Active t (Maybe a) -> m ()
activeMaybe f (Static ma) = maybe blank f ma
activeMaybe f (Dynamic dma) = void $ dyn $ maybe blank f <$> dma

runActive :: (MonadWidget t m, UI t m a) => Active t a -> m ()
runActive (Dynamic a) = void $ dyn $ ui_ <$> a
runActive (Static a) = ui_ a

zipActiveWith :: Reflex t => (a -> b -> c) -> Active t a -> Active t b -> Active t c
zipActiveWith f a b = f <$> a <*> b

-- Attrs

boolClass :: Reflex t => Text -> Active t Bool -> Active t (Maybe Text)
boolClass t a = fmap f a
  where f True = Just t
        f False = Nothing

activeClasses :: Reflex t => [ Active t (Maybe Text) ] -> Active t Classes
activeClasses = fmap (Classes . S.fromList . catMaybes) . sequenceA

newtype Classes = Classes (Set Text)

instance IsString Classes where
  fromString str = Classes $ S.singleton $ fromString str

classAttr :: Classes -> Map Text Text
classAttr (Classes s)
  | S.null s = mempty
  | otherwise = "class" =: foldr (\x acc -> x <> " " <> acc) "" s

addClass :: Text -> Classes -> Classes
addClass t (Classes s) = Classes $ S.insert t s

addClassMaybe :: Maybe Text -> Classes -> Classes
addClassMaybe Nothing c = c
addClassMaybe (Just t) c = addClass t c

addActiveClass :: Reflex t => Active t (Maybe Text) -> Active t Classes -> Active t Classes
addActiveClass c cs = addClassMaybe <$> c <*> cs

instance Semigroup Classes where
  Classes a <> Classes b = Classes $ a <> b

instance Monoid Classes where
  mempty = Classes mempty
  Classes a `mappend` Classes b = Classes $ a `mappend` b


data Style = Style (Map Text Text) deriving (Eq, Show)

styleAttr :: Style -> Map Text Text
styleAttr (Style m)
  | M.null m = mempty
  | otherwise = "style" =: M.foldlWithKey' f "" m
  where f acc k x = acc <> "; " <> k <> ": " <> x


instance Semigroup Style where
  Style a <> Style b = Style $ a <> b

instance Monoid Style where
  mempty = Style mempty
  Style a `mappend` Style b = Style $ a `mappend` b

styleText :: Style -> Text
styleText (Style m)
  = M.foldlWithKey' (\a k b -> a <> "; " <> k <> ": " <> b) "" m

addStyle :: Text -> Text -> Style -> Style
addStyle name value (Style m) = Style $ M.insert name value m


--type Attrs = Attrs $ Map Text Text



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

class ToInline a where
  toInline :: a -> a

--class (ToInline a, UI t m a) => InlineContent t m a where
--  putInline' :: MonadWidget t m => a -> m (El t, Return t m a)
--  putInline' = ui' . toInline

data RenderWhen t a
  = NeverRender
  | AlwaysRender a
  | RenderWhen (Dynamic t Bool) a

isNeverRender :: RenderWhen t a -> Bool
isNeverRender NeverRender = True
isNeverRender _ = False

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


type family Append (as :: [Type]) (bs :: [Type]) :: [Type] where
  Append '[] bs = bs
  Append (a ': as) bs = a ': (Append as bs)

class HListAppend as bs where
  hlistAppend :: HList as -> HList bs -> HList (Append as bs)

instance HListAppend '[] bs where
  hlistAppend HNil bs = bs
instance (Append (a ': as) bs ~ (a ': Append as bs), HListAppend as bs) => HListAppend (a ': as) bs where
  hlistAppend (a `HCons` as) bs = a `HCons` hlistAppend as bs

---------

(|~) :: (Applicative (f t'), Reflex t') => ASetter s t a (f t' b) -> b -> s -> t
l |~ b = set l (pure b)

(|?~) :: (Applicative (f t'), Reflex t') => ASetter s t a (f t' (Maybe b)) -> b -> s -> t
l |?~ b = set l (pure $ Just b)

-- | Like 'count', but keeps the most recent event
data CountWithLast a = NotFired | Fired Int a deriving (Eq, Show)
countWithLast
  :: (Reflex t, MonadHold t m, MonadFix m)
  => Event t a -> m (Dynamic t (CountWithLast a))
countWithLast = holdDyn NotFired <=< zipListWithEvent Fired [1..]

-- | Showing dynamic items such as records with dynamic or event fields
class DynShow t a where
  dynShow :: (Reflex t, MonadHold t m, MonadFix m) => a -> m (Dynamic t String)

data Floated = LeftFloated | RightFloated deriving (Eq, Show)

instance ToClassText Floated where
  toClassText LeftFloated = "left floated"
  toClassText RightFloated = "right floated"

data Size = Mini | Tiny | Small | Medium | Large | Big | Huge | Massive
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

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
  toClassText (Horizontally h) = toClassText h
  toClassText (Vertically v) = toClassText v

instance ToClassText VerticalAttached where
  toClassText TopAttached = "top attached"
  toClassText BottomAttached = "bottom attached"

instance ToClassText HorizontalAttached where
  toClassText LeftAttached = "left attached"
  toClassText RightAttached = "right attached"

combineAttached :: Maybe VerticalAttached -> Maybe HorizontalAttached -> Maybe Text
combineAttached Nothing Nothing = Nothing
combineAttached mv mh = Just $ T.unwords $ catMaybes
  [ vClass <$> mv, hClass <$> mh, Just "attached" ]
  where
    vClass TopAttached = "top"
    vClass BottomAttached = "bottom"
    hClass LeftAttached = "left"
    hClass RightAttached = "right"


data Aligned = LeftAligned | CenterAligned | RightAligned | Justified
  deriving (Eq, Show)

instance ToClassText Aligned where
  toClassText LeftAligned = "left aligned"
  toClassText CenterAligned = "center aligned"
  toClassText RightAligned = "right aligned"
  toClassText Justified = "justified"

data Color
  = Red | Orange | Yellow | Olive | Green | Teal | Blue | Violet | Purple
  | Pink | Brown | Grey | Black
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

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
