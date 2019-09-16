module Combinator where

import           Control.Monad
import           Data.Char
import           Data.String   (String)
import           GHC.Base

type FailureMessage = String

data Result a
  = Success a
  | Failure FailureMessage
  deriving (Show)

success = Success

instance (Eq m) => Eq (Result m) where
  (Success a) == (Success b) = a == b
  (Failure a) == (Failure b) = a == b
  (Success a) /= (Success b) = a /= b
  (Failure a) /= (Failure b) = a == b

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

x |>> f = fmap f x

parseChar :: Char -> Parser String
parseChar matchedChar =
  Parser
    (\input ->
       if head input == matchedChar
         then Success ([head input], tail input)
         else Failure $ "Expect \"a\" but got" ++ "'" ++ [head input] ++ "'")

orElse :: Parser String -> Parser String -> Parser String
orElse firsParser secondParser =
  Parser
    (\input ->
       case run firsParser input of
         Success (parsedData, otherInput) -> Success (parsedData, otherInput)
         Failure msg                      -> run secondParser input)

(<||>) :: Parser String -> Parser String -> Parser String
a <||> b = a `orElse` b

run :: Parser a -> String -> Result (a, String)
run parser input =
  let Parser innerFn = parser
   in innerFn input

--  run (parseA `andThen` parseA)  "aabc" -> Success (["a","a"],"bc")
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

(.>>.) :: Parser String -> Parser String -> Parser (String, String)
a .>>. b = a `andThen` b

choice :: [Parser String] -> Parser String
choice listOfParsers@(x:xy) = foldl (<||>) x xy

anyOf chars = undefined
