module Main where

import           Control.Monad.Writer
import           Data.Char
import           Data.String          (String)
import           Lib

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

newtype Parser a =
  Parser (String -> Result (a, String))

parseA :: Parser String
parseA =
  Parser
    (\input ->
       case head input of
         'a' -> Success ([head input], tail input)
         _ -> Failure $ "Expect \"a\" but got" ++ "'" ++ [head input] ++ "'")

run :: Parser a -> String -> Result (a, String)
run parser input =
  let Parser innerFn = parser
   in innerFn input

--  run (parseA `andThen` parseA)  "aabc" -> Success (["a","a"],"bc")
andThen :: Parser String -> Parser String -> Parser [String]
andThen firstParser secondParser =
  Parser
    (\input ->
       case run firstParser input of
         Failure failMessage -> Failure failMessage
         Success (parsed, otherInput) ->
           case run secondParser otherInput of
             Failure failMessage -> Failure failMessage
             Success (parsed', otherInput') -> Success ([parsed, parsed'], otherInput'))

main :: IO ()
main = print $ run ( parseA `andThen` parseA) "aabc"
