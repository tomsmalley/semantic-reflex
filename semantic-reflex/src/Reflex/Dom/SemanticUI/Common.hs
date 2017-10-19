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
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE TypeInType     #-}
{-# LANGUAGE FunctionalDependencies     #-}

module Reflex.Dom.SemanticUI.Common where

------------------------------------------------------------------------------
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Writer hiding ((<>))
import Control.Lens ((^.), set, ASetter)
import Control.Monad (void, (<=<))
import Data.String
import Data.Semigroup
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Language.Javascript.JSaddle hiding (Success)
import Reflex.Dom.Core hiding (Link, Error, elAttr', DynamicWriterT)

import Reflex.Dom.Active
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- JSaddle helpers

-- | Javascript console.log
consoleLog :: MonadJSM m => ToJSVal a => a -> m ()
consoleLog a = liftJSM $ do
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

newtype Component r m a = Component
  { runComponent :: m a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadSample t, MonadHold t, TriggerEvent t, MonadIO, PostBuild t, MonadReader r')

#ifndef ghcjs_HOST_OS
deriving instance MonadJSM m => MonadJSM (Component r m)
#endif

instance PerformEvent t m => PerformEvent t (Component None m) where
  type Performable (Component None m) = Performable m
  performEvent = Component . performEvent
  performEvent_ = Component . performEvent_

reComponent :: Component r' m a -> Component r m a
reComponent (Component m) = Component m

unComponent :: Component None m a -> Component r m a
unComponent (Component m) = Component m

mapComponent :: forall r t m a b. Monad m
             => (m a -> m b) -> Component r m a -> Component r m b
mapComponent f = Component . f . runComponent

--

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

activeText :: (PostBuild t m, DomBuilder t m, MonadHold t m, MonadFix m)
           => Active t Text -> Component None m ()
activeText (Static t) = Component $ text t
activeText (Dynamic t) = Component $ dynText t

activeMaybe :: (PostBuild t m, DomBuilder t m) => (a -> m ()) -> Active t (Maybe a) -> m ()
activeMaybe f (Static ma) = maybe blank f ma
activeMaybe f (Dynamic dma) = void $ dyn $ maybe blank f <$> dma

{-
runActive :: (Componention r a, MonadWidget t m, UI t m a)
          => Active t a -> Component r m ()
runActive (Dynamic a) = Component $ void $ dyn $ runComponent . ui_ <$> a
runActive (Static a) = ui_ a
-}

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
  | otherwise = "style" =: M.foldrWithKey f "" m
  where f k x acc = k <> ": " <> x <> "; " <> acc


instance Semigroup Style where
  Style a <> Style b = Style $ a <> b

instance Monoid Style where
  mempty = Style mempty
  Style a `mappend` Style b = Style $ a `mappend` b

styleText :: Style -> Text
styleText (Style m)
  = M.foldlWithKey' (\a k b -> a <> "; " <> k <> ": " <> b) "" m

addStyle :: Text -> Text -> Style -> Style
addStyle name v (Style m) = Style $ M.insert name v m

staticText :: (DomBuilder t m, MonadHold t m, MonadFix m) => Text -> Component Inline m ()
staticText t = Component $ text t

widgetHold' :: (MonadHold t m, DomBuilder t m, MonadFix m) => Component r m a -> Event t (Component r m a) -> Component r m (Dynamic t a)
widgetHold' (Component m) evt = Component $ widgetHold m $ fmap runComponent evt

--type Attrs = Attrs $ Map Text Text

data Inline
data None

---------

(|~) :: (Applicative (f t'), Reflex t') => ASetter s t a (f t' b) -> b -> s -> t
l |~ b = set l (pure b)

infixr 4 |~

(|?~) :: (Applicative (f t'), Reflex t') => ASetter s t a (f t' (Maybe b)) -> b -> s -> t
l |?~ b = set l (pure $ Just b)

infixr 4 |?~

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

data Width = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Eleven | Twelve
  deriving (Eq, Ord, Read, Show, Bounded)

instance Enum Width where

  toEnum 2  = Two
  toEnum 3  = Three
  toEnum 4  = Four
  toEnum 5  = Five
  toEnum 6  = Six
  toEnum 7  = Seven
  toEnum 8  = Eight
  toEnum 9  = Nine
  toEnum 10 = Ten
  toEnum 11 = Eleven
  toEnum 12 = Twelve
  toEnum _ = error "Width enum out of bounds"

  fromEnum Two    = 2
  fromEnum Three  = 3
  fromEnum Four   = 4
  fromEnum Five   = 5
  fromEnum Six    = 6
  fromEnum Seven  = 7
  fromEnum Eight  = 8
  fromEnum Nine   = 9
  fromEnum Ten    = 10
  fromEnum Eleven = 11
  fromEnum Twelve = 12

instance ToClassText Width where
  toClassText Two = "two"
  toClassText Three = "three"
  toClassText Four = "four"
  toClassText Five = "five"
  toClassText Six = "six"
  toClassText Seven = "seven"
  toClassText Eight = "eight"
  toClassText Nine = "nine"
  toClassText Ten = "ten"
  toClassText Eleven = "eleven"
  toClassText Twelve = "twelve"

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
data VerticalAttached = TopAttached | Attached | BottomAttached deriving (Eq, Show)

data ExclusiveAttached
  = Horizontally HorizontalAttached
  | Vertically VerticalAttached
  deriving (Eq, Show)

instance ToClassText ExclusiveAttached where
  toClassText (Horizontally h) = toClassText h
  toClassText (Vertically v) = toClassText v

instance ToClassText VerticalAttached where
  toClassText TopAttached = "top attached"
  toClassText Attached = "attached"
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
    vClass Attached = ""
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

data Social
  = Facebook | Twitter | GooglePlus | VK | LinkedIn | Instagram | YouTube
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToClassText Social where
  toClassText Facebook = "facebook"
  toClassText Twitter = "twitter"
  toClassText GooglePlus = "google plus"
  toClassText VK = "vk"
  toClassText LinkedIn = "linkedin"
  toClassText Instagram = "instagram"
  toClassText YouTube = "youtube"

data Emphasis = Primary | Secondary | Tertiary
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToClassText Emphasis where
  toClassText Primary = "primary"
  toClassText Secondary = "secondary"
  toClassText Tertiary = "tertiary"

data Positive = Positive | Negative | Success | Error
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToClassText Positive where
  toClassText Positive = "positive"
  toClassText Negative = "negative"
  toClassText Success = "success"
  toClassText Error = "error"

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
