module Main where

-- Parser
import Parser.Monad
import Parser.Alex

-- bytestring
import qualified Data.ByteString as ByteString


main :: IO ()
main = do
  input <- ByteString.readFile "input.seb"
  case (runParser lexTokens input) of
    Left err -> print err
    Right tokens -> print tokens
