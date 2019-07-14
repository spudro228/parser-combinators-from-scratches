import           Combinator
import           Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "Result data typet" $ do
      it "Success 'a' equal Success 'a' " $ do Success "a" `shouldBe` Success "a"
      it "Success ('a', 'bc') equal Success ('a', 'bc')" $ do Success ("a", "bc") `shouldBe` Success ("a", "bc")
    describe "\"Or\" [<||>] combinator" $ do
      it "use first parser" $ do (run (parseChar 'a' `orElse` parseChar 'b') "abc") `shouldBe` Success ("a", "bc")
      it "use alternative parser" $ do (run (parseChar 'a' <||> parseChar 'b') "bca") `shouldBe` Success ("b", "ca")
    describe "\"And\' [.>>.] combinator" $ do
      it "use both parser" $ do (run (parseChar 'a' .>>. parseChar 'b') "abc") `shouldBe` Success (("a", "b"), "c")
    describe "Parser functor [|>>]" $ do
      it "apply sume function for prsing result, use \"fmap\"" $ do
        (run (fmap (++ "a") (parseChar 'a')) "abc") `shouldBe` Success ("aa", "bc")
      it "apply sume function for prsing result, use [|>>]" $ do
        (run ((parseChar 'a') |>> (++ "a")) "abc") `shouldBe` Success ("aa", "bc")
