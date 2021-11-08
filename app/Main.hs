module Main where

import System.Environment (getArgs)
import qualified Data.ByteString as B (getContents)
import Data.Mailcap (parseMailcapfile)

main :: IO ()
main = do
  contents <- B.getContents
  let parsed = parseMailcapfile contents
  print $ show parsed
