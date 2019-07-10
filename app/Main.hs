module Main where

import           Control.Monad
import           Control.Monad.Writer
import           Data.Char
import           Data.String          (String)
import           GHC.Base

fact :: Integer -> Writer String Integer
fact 0 = return 1
fact n = do
  let n1 = n - 1
  tell $ "First way:" ++ show n ++ "\n"
  m <- fact n1
  tell $ "Second way:" ++ show m ++ "\n"
  let r = m * n
  tell $ "Result: " ++ show r ++ "\n"
  return r

type FailureMessage = String

data Result a
  = Success a
  | Failure FailureMessage
  deriving (Show)

instance Functor Result where
  fmap f (Success x)   = Success (f x)
  fmap _ (Failure msg) = Failure msg

instance Applicative Result where
  pure = Success
  Failure msg <*> _ = Failure msg
  (Success f) <*> a = fmap f a

instance Monad Result where
  a >>= f =
    case a of
      Success x -> f x
      Failure y -> Failure y

newtype Parser a =
  Parser (String -> Result (a, String))

instance Functor Parser where
  fmap f parser =
    Parser
      (\input ->
         let result = run parser input
          in case result of
               Success (value, remaining) -> Success (f value, remaining)
               Failure msg                -> Failure msg)

(|>) x f = fmap f x

parseChar :: Char -> Parser String
parseChar matchedChar =
  Parser
    (\input ->
       if head input == matchedChar
         then Success ([head input], tail input)
         else Failure $ "Expect \"a\" but got" ++ "'" ++ [head input] ++ "'")

--       case head input of
--         matchedChar -> Success ([head input], tail input)
--         _ ->
orElse :: Parser String -> Parser String -> Parser String
orElse firsParser secondParser =
  Parser
    (\input ->
       case run firsParser input of
         Success (parsedData, otherInput) -> Success (parsedData, otherInput)
         Failure msg                      -> run secondParser input)

(<|>) a b = a orElse b

andThen :: Parser String -> Parser String -> Parser (String, String)
andThen firstParser secondParser =
  Parser
    (\input ->
       case run firstParser input of
         Failure failMessage -> Failure failMessage
         Success (parsed, otherInput) ->
           case run secondParser otherInput of
             Failure failMessage -> Failure failMessage
             Success (parsed', otherInput') -> Success ((parsed, parsed'), otherInput'))

(.>>.) a b = a andThen b

--orElse :: Parser String -> Parser String -> Parser a
--orElse firstParser secondParse =
--  Parser
--    (\input ->
--       case run firstParser input of
--         Success (parser, otherInput) -> Success (parser, otherInput)
--         Failure msg                  -> run secondParse input)
--instance Alternative Parser
--  empty = Parser (\(x) -> x)
--  p1 <|> p2 = orElse p1 p2
run :: Parser a -> String -> Result (a, String)
run parser input =
  let Parser innerFn = parser
   in innerFn input

--  run (parseA `andThen` parseA)  "aabc" -> Success (["a","a"],"bc")

main :: IO ()
main = print $ run (parseChar 'a' `andThen` parseChar 'a') "aabc"
