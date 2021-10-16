module Main where

import System.Environment (getArgs)
import qualified Data.Text.IO as T (getContents)
import Data.Mailcap (parseMailcapfile)

main :: IO ()
main = do
  print "accepting input"
  contents <- T.getContents
  print "parsing"
  let parsed = parseMailcapfile contents
  print $ show parsed
