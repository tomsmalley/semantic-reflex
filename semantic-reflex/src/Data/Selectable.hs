{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Selectable where

import Data.Foldable (toList)
import Data.Functor.Identity
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Sequence (Seq, viewr, ViewR(..), (|>), viewl, ViewL(..), (<|))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

import Prelude hiding (length)

-- | Data structures which can select an element. This is similar to an insert
-- operation on a structure, but with the caveat that if the element already
-- exists in the structure it should be removed (if possible). In pseudo code it
-- should satisfy:
--
--    select x xs == if elem x xs then delete x xs else insert x xs
--
class Selectable t where
  selectElement :: Ord a => a -> t a -> t a

instance Selectable [] where
  selectElement a [] = [a]
  selectElement a (a':as)
    | a == a' = as
    | otherwise = a' : selectElement a as

instance Selectable Set where
  selectElement a s
    | Set.member a s = Set.delete a s
    | otherwise = Set.insert a s

instance Selectable NonEmpty where
  selectElement a (a' :| [])
    | a == a' = a :| []
    | otherwise = a' :| [a]

  selectElement a (a' :| as)
    | a == a' = NonEmpty.fromList as
    | otherwise = a' :| selectElement a as

instance Selectable Maybe where
  selectElement a Nothing = Just a
  selectElement a (Just a')
    | a == a' = Nothing
    | otherwise = Just a

instance Selectable Identity where
  selectElement a _ = Identity a

instance Selectable Seq where
  selectElement a (viewl -> EmptyL) = Seq.singleton a
  selectElement a (viewl -> a' :< as)
    | a == a' = as
    | otherwise = a' <| selectElement a as


data LimitedSeq a = LimitedSeq
  { limit :: Int
  , cycling :: Bool
  , list :: Seq a
  } deriving (Eq, Ord, Show)

instance Foldable LimitedSeq where
  foldMap f ll = foldMap f (list ll)

cons :: a -> LimitedSeq a -> LimitedSeq a
cons a ll@LimitedSeq{..}
  | Seq.length list < limit = ll { list = a <| list }
  | cycling, start :> _ <- viewr list = ll { list = a <| start }
  | otherwise = ll

snoc :: a -> LimitedSeq a -> LimitedSeq a
snoc a ll@LimitedSeq{..}
  | Seq.length list < limit = ll { list = list |> a }
  | cycling, _ :< end <- viewl list = ll { list = end |> a }
  | otherwise = ll

instance Selectable LimitedSeq where
  selectElement a ll@LimitedSeq{..}
    | elem a list = ll { list = Seq.fromList $ List.delete a $ toList list }
    | otherwise = snoc a ll

