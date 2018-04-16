{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}

module Reflex.Dom.SemanticUI.Common where

import Control.Lens (set, ASetter)
import Control.Monad (void, guard)
import Data.String
import Data.Semigroup
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core hiding (Link, Error, elAttr', DynamicWriterT)
import Reflex.Active

import qualified Control.Concurrent.Thread.Delay as Concurrent
import Control.Concurrent
import Control.Monad.Fix
import Data.Time
import Data.Sequence as Seq
import Data.Align
import Data.These
import Control.Monad.IO.Class (MonadIO(..))
import System.Random (randomRIO)

-- | Generate a stream of events, triggered by the given event, and separated by
-- /at least/ the given time. The returned event will fire immediately upon
-- triggering, and will contain the triggering event's value and the current
-- event count (counts from @0@ to @n - 1@).
--
-- For example: after waiting enough time, the result of the expression:
--
-- > foldDyn (:) [] =<< echo_ n t =<< getPostBuild
--
-- Will be a list equal to @enumFromTo (n - 1) (n - 2) 0@.
echo
  :: (PerformEvent t m, TriggerEvent t m, MonadIO (Performable m))
  => Int              -- ^ How many events there should be
  -> NominalDiffTime  -- ^ Minimum time separating the events
  -> Event t a        -- ^ Triggering event
  -> m (Event t (a, Int))
echo n t trigger = fmap getLast <$> go 0 trigger where
  go !i evt
    | i < n = do
      eNext <- delay t evt
      ((Last . (,i) <$> evt) <>) <$> go (succ i) eNext
    | otherwise = pure never

-- | Like 'echo', but throws away the triggering event's value.
echo_
  :: (PerformEvent t m, TriggerEvent t m, MonadIO (Performable m))
  => Int              -- ^ How many events there should be
  -> NominalDiffTime  -- ^ Minimum time separating the events
  -> Event t a        -- ^ Triggering event
  -> m (Event t Int)
echo_ n t trigger = fmap getLast <$> go 0 trigger where
  go !i evt
    | i < n = do
      eNext <- delay t evt
      ((Last i <$ evt) <>) <$> go (succ i) eNext
    | otherwise = pure never

-- | Internal type for 'batchOccurrencesImmediate'.
data Some a = None | Only a | Some (Seq a)

-- | When the given 'Event' occurs, wait the given amount of time and collect
-- all occurrences during that time, not including the initial event.
-- Then, fire the returned 'Event' with the collected values. The initial
-- triggering event will be returned immediately in a single value 'Seq'.
batchOccurrencesImmediate
  :: forall t m a.
    ( MonadHold t m, PerformEvent t m, TriggerEvent t m
    , MonadFix m, MonadIO (Performable m))
  => NominalDiffTime -> Event t a -> m (Event t (Seq a))
batchOccurrencesImmediate t newValues = do
  rec (buffer, toDelay) <- mapAccumMaybe f None $ align newValues delayed
      delayed <- delaySelf_ toDelay
  pure $ fforMaybe (tag buffer delayed) $ \case
    None -> Nothing
    Only a -> Just $ pure a
    Some s -> if Seq.null s then Nothing else Just s
  where
    f s t' = let (s', m) = g s t' in (Just s', m)
    g :: Some a -> These a () -> (Some a, Maybe NominalDiffTime)
    g None (This a) = (Only a, Just 0)
    g None (That _) = (None, Nothing)
    g None (These a _) = g None (This a)
    g (Only _) (This a) = (Some $ pure a, Nothing)
    g (Only _) (That _) = (Some mempty, Just t)
    g (Only _) (These a _) = (fst $ g (Some mempty) (This a), Just t)
    g (Some s) (This a) = (Some $ s |> a, Nothing)
    g (Some s) (That _)
      | Seq.null s = (None, Nothing)
      | otherwise = (Some mempty, Just t)
    g (Some s) (These a _)
      | Seq.null s = g None (This a)
      | otherwise = g (Some mempty) (This a)

-- | Prevent a 'Dynamic' from updating faster than the given time step.
rateLimitDyn
  :: ( MonadSample t m, PostBuild t m, MonadHold t m, MonadFix m
     , PerformEvent t m, TriggerEvent t m, MonadIO (Performable m))
  => NominalDiffTime -> Dynamic t a -> m (Dynamic t a)
rateLimitDyn t d = do
  eUpdate <- batchOccurrencesImmediate t $ updated d
  a <- sample $ current d
  holdDyn a $ tag (current d) eUpdate

-- | Delay individual Event occurrences by the amount given in the event, in seconds.
delaySelf_ :: (PerformEvent t m, TriggerEvent t m, MonadIO (Performable m))
          => Event t NominalDiffTime -> m (Event t ())
delaySelf_ e = performEventAsync $ ffor e $ \dt cb -> liftIO $ void $ forkIO $ do
  Concurrent.delay $ ceiling $ dt * 1000000
  cb ()

-- | Delay individual Event occurrences by the amount given in the event, in seconds.
delaySelf :: (PerformEvent t m, TriggerEvent t m, MonadIO (Performable m))
          => Event t (NominalDiffTime, a) -> m (Event t a)
delaySelf e = performEventAsync $ ffor e $ \(dt, a) cb -> liftIO $ void $ forkIO $ do
  Concurrent.delay $ ceiling $ dt * 1000000
  cb a

-- | Get a random 'Int' from the given range when triggered by the input
-- 'Event'. Uses the global random number generator through 'randomRIO'.
randomREvent
  :: (PerformEvent t m, MonadIO (Performable m)) => (Int, Int) -> Event t () -> m (Event t Int)
randomREvent range trigger = performEvent $ liftIO (randomRIO range) <$ trigger

-- | Handy for filtering events to the given key
keyIs :: Reflex t => Key -> Event t Word -> Event t ()
keyIs key = void . ffilter (\n -> keyCodeLookup (fromIntegral n) == key)

-- | Show 'Text' by just packing the result of 'show'
tshow :: Show a => a -> Text
tshow = T.pack . show

-- | 'divClass' but returning the element
divClass'
  :: DomBuilder t m => Text -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
divClass' = elClass' "div"

------------------------------------------------------------------------------

-- | A class for converting properties to their corresponding CSS class text.
class ToClassText a where
  toClassText :: a -> Text

instance ToClassText a => ToClassText (Maybe a) where
  toClassText Nothing = mempty
  toClassText (Just a) = toClassText a

-- | Test the contents of a 'Maybe' against the given value, if they are equal,
-- return 'Nothing', otherwise return 'Just' the value.
nothingIf :: Eq a => a -> Maybe a -> Maybe a
nothingIf x (Just y) | x == y = Nothing
nothingIf _ m = m

-- | Helper function; returns 'Just' the given CSS class when the predicate is
-- true, 'Nothing' otherwise
boolClass :: Functor f => Text -> f Bool -> f (Maybe Text)
boolClass t = fmap $ \b -> t <$ guard b

-- | Combine a list of dynamic CSS classes into 'Classes'
dynClasses
  :: Reflex t => [Active t (Maybe Text)] -> Active t Classes
dynClasses = distributeListOverActiveWith (<>) $ Classes . catMaybes

-- | Combine a list of dynamic CSS classes into 'Classes'
dynClasses' :: Reflex t => [Dynamic t (Maybe Text)] -> Dynamic t Classes
dynClasses' = distributeListOverDynWith $ Classes . catMaybes

-- | Element classes
newtype Classes = Classes [Text] deriving (Eq, Show)

instance IsString Classes where
  fromString str = Classes $ pure $ fromString str

instance Semigroup Classes where
  Classes a <> Classes b = Classes $ a <> b

instance Monoid Classes where
  mempty = Classes mempty
  Classes a `mappend` Classes b = Classes $ a `mappend` b

getClasses :: Classes -> Text
getClasses (Classes []) = ""
getClasses (Classes cs) = T.unwords cs

-- | Make the "class" attribute from 'Classes'
classAttr :: Classes -> Map Text Text
classAttr (Classes []) = mempty
classAttr (Classes cs) = "class" =: T.unwords cs

-- | Helper for adding a class to a 'Classes'
addClass :: Text -> Classes -> Classes
addClass c (Classes cs) = Classes $ c : cs

-- | CSS styles
newtype Style = Style Text deriving (Eq, Show)

instance IsString Style where
  fromString = Style . fromString

instance Semigroup Style where
  Style "" <> Style b = Style b
  Style a <> Style "" = Style a
  Style a <> Style b = Style $ a <> ";" <> b

instance Monoid Style where
  mempty = Style ""
  mappend = (<>)

getStyle :: Style -> Text
getStyle (Style t) = t

-- | Make the "style" attribute from 'Style'
styleAttr :: Style -> Map Text Text
styleAttr (Style "") = mempty
styleAttr (Style s) = "style" =: s

-- | Set the target of a 'Lens', 'Traversal' or 'Setter' to 'pure' a value.
--
-- @
-- l '|~' t ≡ 'set' l ('pure' t)
-- @
--
(|~) :: (Applicative (f t'), Reflex t') => ASetter s t a (f t' b) -> b -> s -> t
l |~ b = set l (pure b)
infixr 4 |~

-- | Set the target of a 'Lens', 'Traversal' or 'Setter' to 'pure . Just' a value.
--
-- @
-- l '|?~' t ≡ 'set' l ('pure' '$' 'Just' t)
-- @
--
(|?~) :: (Applicative (f t'), Reflex t') => ASetter s t a (f t' (Maybe b)) -> b -> s -> t
l |?~ b = set l (pure $ Just b)
infixr 4 |?~

-- | The side of a label
data Labeled = LeftLabeled | RightLabeled
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToClassText Labeled where
  toClassText LeftLabeled = "left labeled"
  toClassText RightLabeled = "right labeled"

-- | The side of floated content
data Floated = LeftFloated | RightFloated
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToClassText Floated where
  toClassText LeftFloated = "left floated"
  toClassText RightFloated = "right floated"

-- | 12 column widths
data Width = Two | Three | Four | Five | Six | Seven
           | Eight | Nine | Ten | Eleven | Twelve
  deriving (Eq, Ord, Read, Show, Bounded)

-- | Enumerated from 2: 'toEnum 2 = Two'...
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

-- | (Almost) universal size property
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

-- | Horizontal attachment side
data HorizontalAttached = LeftAttached | RightAttached
  deriving (Eq, Ord, Read, Show, Enum, Bounded)
-- | Vertical attachment side. Things can also be sandwiched between other
-- vertical attachments, these are just 'Attached'.
data VerticalAttached = TopAttached | Attached | BottomAttached
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

-- | In some cases things can be either horizontally *or* vertically attached.
data ExclusiveAttached
  = Horizontally HorizontalAttached
  | Vertically VerticalAttached
  deriving (Eq, Ord, Read, Show)

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

-- | Text alignment
data Aligned = LeftAligned | CenterAligned | RightAligned | Justified
  deriving (Eq, Show)

instance ToClassText Aligned where
  toClassText LeftAligned = "left aligned"
  toClassText CenterAligned = "center aligned"
  toClassText RightAligned = "right aligned"
  toClassText Justified = "justified"

-- | Supported social network branding
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

-- | Typical emphasis levels
data Emphasis = Primary | Secondary | Tertiary
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToClassText Emphasis where
  toClassText Primary = "primary"
  toClassText Secondary = "secondary"
  toClassText Tertiary = "tertiary"

-- | 'Positive' can provide fast visual feedback of interactions. With the
-- default theme, 'Positive' and 'Success' will usually look the same, as well
-- ase 'Negative' and 'Error'.
data Positive = Positive | Negative | Success | Error
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToClassText Positive where
  toClassText Positive = "positive"
  toClassText Negative = "negative"
  toClassText Success = "success"
  toClassText Error = "error"

-- | Supported colour range
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
