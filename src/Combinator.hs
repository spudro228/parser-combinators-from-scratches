module Combinator where

import           Control.Monad
import           Data.Char
import           Data.String          (String)
import           GHC.Base


type FailureMessage = String

data Result a
  = Success a
  | Failure FailureMessage
  deriving (Show)

success a = Success a



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

(|>>) x f = map f x

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

(<||>) :: Parser String -> Parser String -> Parser String
a <||> b = a `orElse` b
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
--retSuccess :: String -> Result String
--retSuccess s =
--  if s == "s"
--    then Success s
--    else Failure "fail"
--fff i = do
--  x <- retSuccess i
--  y <- retSuccess x
--  return [x, y]