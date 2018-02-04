{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Reflex.Dom.SemanticUI.Common where

import Control.Lens (set, ASetter)
import Control.Monad (void, guard)
import Data.String
import Data.Semigroup
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core hiding (Link, Error, elAttr', DynamicWriterT)

-- | Handy for filtering events to the given key
keyIs :: Reflex t => Key -> Event t Word -> Event t ()
keyIs key = void . ffilter (\n -> keyCodeLookup (fromIntegral n) == key)

{-# INLINABLE keydown #-}
keydown
  :: (Reflex t, HasDomEvent t e 'KeydownTag, DomEventType e 'KeydownTag ~ Word)
  => Key -> e -> Event t ()
keydown key = fmapMaybe (\n -> guard $ keyCodeLookup (fromIntegral n) == key)
            . domEvent Keydown

{-# INLINABLE keyup #-}
keyup
  :: (Reflex t, HasDomEvent t e 'KeyupTag, DomEventType e 'KeyupTag ~ Word)
  => Key -> e -> Event t ()
keyup key = fmapMaybe (\n -> guard $ keyCodeLookup (fromIntegral n) == key)
          . domEvent Keyup

-- | Show 'Text' by just packing the result of 'show'
tshow :: Show a => a -> Text
tshow = T.pack . show

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
dynClasses :: Reflex t => [Dynamic t (Maybe Text)] -> Dynamic t Classes
dynClasses = distributeListOverDynWith $ Classes . S.fromList . catMaybes

-- | Classes can be modelled as a 'Set' of 'Text'. Since in some cases ordering
-- does count with Semantic-UI, these should be encoded in a single item such
-- as 'Classes (fromList ["left aligned"])'.
newtype Classes = Classes (Set Text) deriving (Eq, Show)

instance IsString Classes where
  fromString str = Classes $ S.singleton $ fromString str

instance Semigroup Classes where
  Classes a <> Classes b = Classes $ a <> b

instance Monoid Classes where
  mempty = Classes mempty
  Classes a `mappend` Classes b = Classes $ a `mappend` b

-- | Make the "class" attribute from 'Classes'
classAttr :: Classes -> Map Text Text
classAttr (Classes s)
  | S.null s = mempty
  | otherwise = "class" =: foldr (\x acc -> x <> " " <> acc) "" s

-- | Helper for adding a class to a 'Classes'
addClass :: Text -> Classes -> Classes
addClass t (Classes s) = Classes $ S.insert t s

-- | CSS styles are modelled similar to attributes, a 'Map' from 'Text'
-- properties to 'Text' values.
newtype Style = Style (Map Text Text) deriving (Eq, Show)

instance Semigroup Style where
  Style a <> Style b = Style $ a <> b

instance Monoid Style where
  mempty = Style mempty
  Style a `mappend` Style b = Style $ a `mappend` b

-- | Make the "style" attribute from 'Style'
styleAttr :: Style -> Map Text Text
styleAttr (Style m)
  | M.null m = mempty
  | otherwise = "style" =: T.concat (map f $ M.toList m)
  where f (k, x) = T.concat [k, ":", x <> ";"]

-- | 'divClass' but returning the element
divClass' :: (DomBuilderSpace m ~ GhcjsDomSpace, DomBuilder t m) => Text -> m a -> m (El t, a)
divClass' = elClass' "div"

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
