module Main where

import System.Environment (getArgs)
import qualified Data.ByteString as B (getContents)
import Data.Mailcap (parseMailcapfile)

main :: IO ()
main = do
  print "Reading from stdin..."
  contents <- B.getContents
  print contents
  print "Showing parsed: "
  let parsed = parseMailcapfile contents
  print $ show parsed
