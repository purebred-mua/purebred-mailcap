{-# LANGUAGE OverloadedStrings #-}

module Data.Mailcap (parseMailcapfile, mailcapfile, mailcapline, mailcapentry, comment, MailcapLine (..), Entry (..), Field (..)) where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (char8, isEndOfLine, skipSpace)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.Mailcap.Internal
import Data.Functor ((<&>), ($>))
import qualified Data.Text as T

type MailcapFile = [MailcapLine]

data MailcapLine
  = Comment B.ByteString
  | MailcapEntry Entry
  deriving (Show, Eq)

data Entry = Entry
  { _contentType :: ContentType,
    _viewCommand :: String,
    _fields :: [Field]
  }
  deriving (Show, Eq)

data Field
  = ComposeTyped String
  | Edit String
  | Print String
  | Test String
  | NeedsTerminal
  | CopiousOutput
  | Description String
  | TextualNewlines
  | X11Bitmap String
  | NameTemplate
  deriving (Show, Eq)

type ViewCommand = T.Text

parseMailcapfile :: B.ByteString -> Either String MailcapFile
parseMailcapfile = parseOnly (mailcapfile <* endOfInput)

mailcapfile :: Parser MailcapFile
mailcapfile = many' mailcapline

mailcapline :: Parser MailcapLine
mailcapline = mailcapentry <|> comment

mailcapentry :: Parser MailcapLine
mailcapentry = do
  ct <- parseContentType
  vc <- skip (== 59) *> skipSpace *> takeTill (== 59) -- 59 == ';'
  skip (== 59) -- TODO
  fields <- field `sepBy` char8 ';'
  pure $ MailcapEntry $ Entry ct (C8.unpack vc) fields

field :: Parser Field
field = skipSpace *> (flag <|> x11bitmap)

x11bitmap :: Parser Field
x11bitmap = (string "x11-bitmap=" *> skipQuote *> takeTill (== 34)) <&> (X11Bitmap . C8.unpack)
  where
    skipQuote :: Parser ()
    skipQuote = skip (== 34)

-- TODO x-token
flag :: Parser Field
flag = skipSpace *> (copiousoutput <|> needsterminal)

needsterminal :: Parser Field
needsterminal = string "needsterminal" $> NeedsTerminal

copiousoutput :: Parser Field
copiousoutput = string "copiousoutput" $> CopiousOutput

comment :: Parser MailcapLine
comment = do
  txt <- string "#" *> skipSpace *> takeTill isEndOfLine
  pure $ Comment txt
