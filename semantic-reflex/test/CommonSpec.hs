{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module CommonSpec where

import Data.Semigroup ((<>))

import Data.Foldable (toList)
import Control.Monad.IO.Class
import Control.Concurrent
import Test.Hspec
import Test.QuickCheck hiding (sample)
import Test.QuickCheck.Instances
import Test.Hspec.QuickCheck
import Control.Concurrent.MVar
import Data.Time.Clock

import Reflex.Dom.SemanticUI
import Language.Javascript.JSaddle.Null

import qualified Data.Map as M
import qualified Data.Sequence as Seq

instance Arbitrary Classes where
  arbitrary = Classes <$> arbitrary

instance Arbitrary Style where
  arbitrary = Style <$> arbitrary

sampleActive :: (MonadSample t m, Reflex t) => Active t a -> m a
sampleActive = \case
  Static a -> pure a
  Dyn d -> sample $ current d

spec :: Spec
spec = do

  describe "Classes" $ do
    it "obeys mempty <> a = a" $ property $ \(c :: Classes) -> mempty <> c == c
    it "obeys a <> mempty = a" $ property $ \(c :: Classes) -> c <> mempty == c
    it "obeys <> law" $ property $ \a b (c :: Classes) -> (a <> b) <> c == a <> (b <> c)
    it "appends correctly" $ do
      Classes ["test"] <> Classes ["classes"] `shouldBe` Classes ["test", "classes"]
    it "renders space separated" $ do
      getClasses (Classes ["test", "classes", "three"]) `shouldBe` "test classes three"
    it "classAttr" $ do
      classAttr mempty `shouldBe` mempty
      classAttr "test" `shouldBe` M.singleton "class" "test"

  describe "Style" $ do
    it "obeys mempty <> a = a" $ property $ \(c :: Style) -> mempty <> c == c
    it "obeys a <> mempty = a" $ property $ \(c :: Style) -> c <> mempty == c
    it "obeys <> law" $ property $ \a b (c :: Style) -> (a <> b) <> c == a <> (b <> c)
    it "styleAttr" $ do
      styleAttr mempty `shouldBe` mempty
      styleAttr (Style "test") `shouldBe` M.singleton "style" "test"
    it "appends correctly with ;" $ do
      Style "color: red" <> Style "width: 0" `shouldBe` Style "color: red;width: 0"

  describe "ButtonConfig" $ do
    it "works with default" $ do
      c <- runSpiderHost $ sampleActive $ buttonConfigClasses def
      c `shouldBe` Classes ["ui button"]
    it "works with toClassText" $ do
      c <- runSpiderHost $ sampleActive $ buttonConfigClasses $ def & buttonConfig_color |?~ Red
      c `shouldBe` Classes ["ui button", "red"]
    it "works with boolClass" $ do
      c <- runSpiderHost $ sampleActive $ buttonConfigClasses $ def & buttonConfig_inverted |~ True
      c `shouldBe` Classes ["ui button", "inverted"]

  describe "Time functions" $ do
    describe "echo" $ do
      it "produces the correct number of events in the correct order" $ do
        es <- collectEventsFor 0.06 $ do
          pb <- getPostBuild
          pbd <- delay 0.005 pb
          echo 5 0.01 $ leftmost [True <$ pb, False <$ pbd]
        es `shouldBe`
          [ (False, 4), (True, 4)
          , (False, 3), (True, 3)
          , (False, 2), (True, 2)
          , (False, 1), (True, 1)
          , (False, 0), (True, 0)
          ]

    describe "echo_" $ do
      it "produces the correct number of events in the correct order" $ do
        es <- collectEventsFor 0.06 $ do
          pb <- getPostBuild
          pbd <- delay 0.005 pb
          echo_ 5 0.01 $ leftmost [pb, pbd]
        es `shouldBe` [4, 4, 3, 3, 2, 2, 1, 1, 0, 0]

--      it "is timed correctly" $ do
--        mvar <- newEmptyMVar
--        run $ mainWidget $ do
--          es <- echo 20 0.001 =<< echo_ 20 0.01 =<< getPostBuild
--          performEvent_ $ ffor es $ liftIO . putMVar mvar
--        let loop t0 = do
--              x <- takeMVar mvar
--              t1 <- getCurrentTime
--              putStrLn $ show x ++ " " ++ show (diffUTCTime t1 t0)
--              case x of
--                (19,19) -> pure ()
--                _ -> loop t0
--        loop =<< getCurrentTime

    let doubleToTime = fromRational . toRational :: Double -> NominalDiffTime
    describe "batchOccurrencesImmediate" $ do
      it "never drops events" $ property $ do
        t' :: Double <- generate $ choose (0,0.001)
        n :: Int <- generate $ choose (0, 10)
        dt' :: Double <- generate $ choose (0, t' * fromIntegral n)
        let t = doubleToTime t'
            dt = doubleToTime dt'
        -- Give the system some extra time
        es <- collectEventsFor (0.01 + fromIntegral n * t) $ do
          batchOccurrencesImmediate dt =<< echo_ n t =<< getPostBuild
        (reverse . toList =<< es) `shouldBe` enumFromThenTo (n - 1) (n - 2) 0

      it "batches correctly" $ do
        es <- collectEventsFor 0.02 $ do
          pb <- getPostBuild
          pb1 <- delay 0.01 pb
          batchOccurrencesImmediate 0.005 $ leftmost
            [(1 :: Int) <$ pb, 2 <$ pb1]
        es `shouldBe` [Seq.singleton 2, Seq.singleton 1]

-- | Run a widget for the specified time, collecting the event firings into a
-- list
collectEventsFor :: NominalDiffTime -> Widget () (Event DomTimeline a) -> IO [a]
collectEventsFor t m = do
  mvar <- newMVar []
  run $ mainWidget' $ do
    e <- m
    performEvent_ $ ffor e $ \a -> liftIO . modifyMVar_ mvar $ pure . (a :)
  threadDelay $ round $ t * 1000000
  takeMVar mvar


