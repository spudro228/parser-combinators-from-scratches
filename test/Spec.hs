import Combinator
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "Result data typet" $ do
      it "Success 'a' equal Success 'a' " $ do
        Success "a" `shouldBe` Success "a"
      it "Success ('a', 'bc') equal Success ('a', 'bc')" $ do
        Success ("a", "bc") `shouldBe` Success ("a", "bc")
