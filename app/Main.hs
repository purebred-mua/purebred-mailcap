module Main where

import qualified Data.Mailcap (mailcapFile)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
