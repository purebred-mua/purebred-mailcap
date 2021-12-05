{-# LANGUAGE OverloadedStrings #-}

module Data.RFC1524 (
    parseMailcapfile
  , mailcapfile
  , mailcapline
  , mailcapentry
  , comment
  , mtext
  , needsterminal
  , MailcapLine (..)
  , Entry (..)
  , Field (..)
  ) where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (char8, isEndOfLine, isSpace_w8, skipSpace, stringCI, space)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.Functor (($>), (<&>))
import Data.Word (Word8)
import qualified Data.Text as T
import qualified Data.CaseInsensitive as CI

import Data.RFC1524.Internal

type MailcapFile = [MailcapLine]

data MailcapLine
  = Comment B.ByteString
  | MailcapEntry Entry
  deriving (Show, Eq)

data Entry = Entry
  { _contentType :: ContentType,
    _viewCommand :: B.ByteString,
    _fields :: [Field]
  }
  deriving (Show, Eq)

data Field
  = Flag B.ByteString
  | Compose B.ByteString
  | ComposeTyped B.ByteString
  | Print B.ByteString
  | Edit B.ByteString
  | Test B.ByteString
  | X11Bitmap B.ByteString
  | TextualNewlines B.ByteString
  | Description B.ByteString
  | XToken
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
  ct <- typefield
  skipMany1 semicolon
  vc <- skipSpace *> viewCommand
  skipMany semicolon
  fields <- field `sepBy` char8 ';'
  pure $ MailcapEntry $ Entry ct vc fields

typefield :: Parser ContentType
typefield = parseContentType

-- TODO should be a shell command
viewCommand :: Parser B.ByteString
viewCommand = mtext

mtext :: Parser B.ByteString
mtext = many' mchar <&> B.pack

mchar :: Parser Word8
mchar = qchar <|> schar

schar :: Parser Word8
schar = satisfy excludeMchar
  where
    excludeMchar :: Word8 -> Bool
    excludeMchar c = c /= 59 && c /= 92

qchar :: Parser Word8
qchar = char8 '\\' *> anyWord8

skipSpaceOrQChar :: Parser ()
skipSpaceOrQChar = skipMany ((space $> ()) <|> (qchar $> ()))

field :: Parser Field
field = skipSpaceOrQChar *> (flag <|> namedfield)

namedfield :: Parser Field
namedfield = composefield <|> test <|> x11bitmap <|> description <|> edit

description :: Parser Field
description = stringCI "description" *> equal *> mtext <&> Description

composefield :: Parser Field
composefield = stringCI "compose" *> equal *> mtext <&> Compose

edit :: Parser Field
edit = stringCI "edit" *> equal *> mtext <&> Edit

test :: Parser Field
test = stringCI "test" *> equal *> mtext <&> Test

x11bitmap :: Parser Field
x11bitmap = stringCI "x11-bitmap" *> equal *> mtext <&> X11Bitmap

-- | Flags
flag :: Parser Field
flag = skipSpace *> (copiousoutput <|> needsterminal)

needsterminal :: Parser Field
needsterminal = stringCI "needsterminal" $> Flag "needsterminal"

copiousoutput :: Parser Field
copiousoutput = stringCI "copiousoutput" $> Flag "copiousoutput"

-- | Comments
comment :: Parser MailcapLine
comment = do
  txt <- string "#" *> skipSpace *> takeTill isEndOfLine
  pure $ Comment txt

-- | Parsing Help
equal :: Parser ()
equal = char8 '=' $> ()

skipQuote :: Parser ()
skipQuote = skip (== 34)

semicolon :: Parser ()
semicolon = char8 ';' $> ()
