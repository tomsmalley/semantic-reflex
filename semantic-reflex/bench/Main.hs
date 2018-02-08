{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-full-laziness #-}

-- | Testing speed differences between dynamic and static dom generation
module Main where

import Control.Applicative (liftA2)
import Control.DeepSeq
import Control.Monad (replicateM_, (<=<), void)
import Control.Monad.Fix (MonadFix)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Reflex hiding (collectDyn)
import Reflex.Dom.SemanticUI hiding (collectDyn)
import Criterion.Main
import System.IO.Unsafe

import qualified Data.Text as T

collectActive :: Reflex t => [Active t (Maybe Text)] -> Active t Text
collectActive = distributeListOverActiveWith joining

collectActive' :: Reflex t => [Active t (Maybe Text)] -> Active t Text
collectActive'
  = distributeListOverActiveWith' (\a b -> T.unwords [a, b]) joining

collectActive'unsafe :: [Active () (Maybe Text)] -> Text
collectActive'unsafe = joining . fst . unzipActive

collectDyn :: Reflex t => [Dynamic t (Maybe Text)] -> Dynamic t Text
collectDyn = distributeListOverDynWith joining

joining :: [Maybe Text] -> Text
joining = T.unwords . catMaybes

unsafeStatic :: Active t a -> a
unsafeStatic (Static a) = a

instance NFData Classes where
  rnf (Classes t) = rnf t

main :: IO ()
main = defaultMain
  [ bgroup "dyn"
    [ bench "joining" $ nf joining samples
    , bench "IO joining" $ nf (unsafePerformIO . pure . joining) samples
    , bench "spider joining" $ nf (unsafePerformIO . runSpiderHost . pure . joining) samples

    , bench "collectActive'unsafe all Static unsafe"
    $ nf collectActive'unsafe (Static <$> samples)

    , bench "collectActive all Static" $ flip nf (pure <$> samples)
    $ \s -> unsafePerformIO . runSpiderHost $
      case collectActive s of
        Static a -> pure a
        Dyn d -> sample $ current d

    , bench "collectActive all Dyn" $ flip nf (Dyn . pure <$> samples)
    $ \s -> unsafePerformIO . runSpiderHost $ do
      case collectActive s of
        Static a -> pure a
        Dyn d -> sample $ current d

    , bench "collectActive 3 Dyn" $ flip nf
    ((\xs -> map (Dyn . pure . unsafeStatic) (take 3 xs) ++ drop 3 xs) $ pure <$> samples)
    $ \s -> unsafePerformIO . runSpiderHost $ do
      case collectActive s of
        Static a -> pure a
        Dyn d -> sample $ current d

    , bench "collectActive' all Static" $ flip nf (pure <$> samples)
    $ \s -> unsafePerformIO . runSpiderHost $
      case collectActive' s of
        Static a -> pure a
        Dyn d -> sample $ current d

    , bench "collectActive' all Dyn" $ flip nf (Dyn . pure <$> samples)
    $ \s -> unsafePerformIO . runSpiderHost $ do
      case collectActive' s of
        Static a -> pure a
        Dyn d -> sample $ current d

    , bench "collectActive' 3 Dyn" $ flip nf
    ((\xs -> map (Dyn . pure . unsafeStatic) (take 3 xs) ++ drop 3 xs) $ pure <$> samples)
    $ \s -> unsafePerformIO . runSpiderHost $ do
      case collectActive' s of
        Static a -> pure a
        Dyn d -> sample $ current d

    , bench "collectDyn" $ flip nf (pure <$> samples)
    $ \s -> unsafePerformIO . runSpiderHost $ do
      sample $ current $ collectDyn s

    , bench "collectDyn single" $ flip nf (pure <$> take 1 samples)
    $ \s -> unsafePerformIO . runSpiderHost $ do
      sample $ current $ collectDyn s

    ]
  ]

samples :: [Maybe Text]
samples = take 20 $ cycle
  [ Nothing, Just "test", Nothing, Just "text", Just "test", Nothing
  , Just "text", Nothing, Nothing, Just "1234", Nothing, Just "0", Just "data" ]

