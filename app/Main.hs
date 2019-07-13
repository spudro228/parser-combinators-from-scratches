module Main where
import Combinator
import           Control.Monad.Writer

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

main :: IO ()
main = print $ run (parseChar 'a' `andThen` parseChar 'a') "aabc"
