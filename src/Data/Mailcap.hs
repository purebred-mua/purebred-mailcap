module Data.Mailcap (
    module Data.RFC1524
  , module Data.RFC1524.ViewCommand
  , parseMailcapfile
  ) where

import qualified Data.Attoparsec.Internal.Types as AT
import Data.Attoparsec.ByteString (Parser, endOfInput, parseOnly, peekWord8')
import Control.Applicative ((<|>))
import qualified Data.ByteString as B
import Data.ByteString.Internal (w2c)

import Data.RFC1524

import Data.RFC1524.ViewCommand

parseMailcapfile :: B.ByteString -> Either String MailcapFile
parseMailcapfile = parseOnly (mailcapfile <* niceEndOfInput)

niceEndOfInput :: Parser()
niceEndOfInput = endOfInput <|> p
  where
    p = do
      c <- peekWord8'
      off <- offset
      fail $ "unexpected " <> show (w2c c) <> " at offset " <> show off

offset :: AT.Parser i Int
offset = AT.Parser $ \t pos more _lose suc -> suc t pos more (AT.fromPos pos)
