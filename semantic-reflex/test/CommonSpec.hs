module CommonSpec where

import Data.Semigroup ((<>))

import Test.Hspec
import Test.QuickCheck (property, Arbitrary(..))
import Test.QuickCheck.Instances
import Test.Hspec.QuickCheck

import Reflex.Dom.SemanticUI

import qualified Data.Map as M

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
      c <- runSpiderHost $ sampleActive $ buttonConfigClasses $ def & buttonColor |?~ Red
      c `shouldBe` Classes ["ui button", "red"]
    it "works with boolClass" $ do
      c <- runSpiderHost $ sampleActive $ buttonConfigClasses $ def & buttonInverted |~ True
      c `shouldBe` Classes ["ui button", "inverted"]

