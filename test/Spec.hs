import Test.Hspec
import Combinator
main :: IO ()
main = hspec $ do
  describe "absolute" $ do
    it "Success equal Success" $ do
      Success "a" `shouldBe` Success "a"