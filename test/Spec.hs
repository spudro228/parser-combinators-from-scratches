import           Combinator
import           Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "Result data typet" $ do
      it "Success 'a' equal Success 'a' " $ do Success "a" `shouldBe` Success "a"
      it "Success ('a', 'bc') equal Success ('a', 'bc')" $ do Success ("a", "bc") `shouldBe` Success ("a", "bc")
    describe "\"orElse\" [<||>] combinator" $ do
      it "use first parser" $ do (run (parseChar 'a' `orElse` parseChar 'b') "abc") `shouldBe` Success ("a", "bc")
      it "use alternative parser" $ do (run (parseChar 'a' <||> parseChar 'b') "bca") `shouldBe` Success ("b", "ca")
    describe "\"andThen\' [.>>.] combinator" $ do
      it "use both parser" $ do (run (parseChar 'a' .>>. parseChar 'b') "abc") `shouldBe` Success (("a", "b"), "c")
    describe "Parser functor [|>>]" $ do
      it "apply sume function for prsing result, use \"fmap\"" $ do
        (run (fmap (++ "a") (parseChar 'a')) "abc") `shouldBe` Success ("aa", "bc")
      it "apply sume function for prsing result, use [|>>]" $ do
        (run ((parseChar 'a') |>> (++ "a")) "abc") `shouldBe` Success ("aa", "bc")
    describe "\"choice\" choice parser from list of parsers" $ do
      it "choice last parser" $ do
        (run (choice [parseChar 'b', parseChar 'c', parseChar 'a']) "abc") `shouldBe` Success ("a", "bc")
      it "choice first parser" $ do
        (run (choice [parseChar 'a', parseChar 'c', parseChar 'b']) "abc") `shouldBe` Success ("a", "bc")
--    describe "\"anyOf\" parse any character from list in string" $ do
--      it "anyOf ['a','f','m']" $ do (run (anyOf ['a', 'm', 'f']) "abc") `shouldBe` Success ("a", "bc")
--      it "anyOf ['a', 'b', 'c']" $ do (run (anyOf ['a', 'b', 'c']) "abc") `shouldBe` Success ("a", "bc")
--      it "anyOf number 9 from ['0' .. '9']" $ do (run (anyOf ['0' .. '9']) "9abc") `shouldBe` Success ("9", "bc")
--      it "anyOf number 96 from ['0' .. '9']" $ do (run (anyOf ['0' .. '9']) "96abc") `shouldBe` Success ("96", "bc")
--      it "dont choice number 9 from ['0' .. '9']" $ do (run (anyOf ['0' .. '9']) "|bc") `shouldBe` Failure "asdasd"
