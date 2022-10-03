{-# LANGUAGE OverloadedStrings #-}

module Data.RFC1524 (
    mailcapfile
  , mailcapline
  , mailcapentry
  , comment
  , mtext
  , needsterminal
  , MailcapLine (..)
  , MailcapFile(..)
  , Entry (..)
  , Field (..)
  ) where

import Prelude hiding (print)
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (char8, isEndOfLine, isSpace_w8, skipSpace, stringCI, space, endOfLine, peekChar')
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.Functor (($>), (<&>))
import Data.Word (Word8)
import qualified Data.Text as T
import qualified Data.CaseInsensitive as CI

import Data.RFC1524.Internal
import Data.RFC1524.ViewCommand

type MailcapFile = [MailcapLine]

data MailcapLine
  = Comment B.ByteString
  | MailcapEntry Entry
  deriving (Show, Eq)

data Entry = Entry
  { _contentType :: ContentType,
    _viewCommand :: ExecutableCommand,
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
  | TextualNewlines Bool
  | Description B.ByteString
  | XToken
  deriving (Show, Eq)

type ViewCommand = T.Text

mailcapfile :: Parser MailcapFile
mailcapfile = many' mailcapline

mailcapline :: Parser MailcapLine
mailcapline = comment <|> mailcapentry

mailcapentry :: Parser MailcapLine
mailcapentry = do
  ct <- typefield
  semicolon
  skipSpace
  vc <- viewCommand <* (endOfLine <|> semicolon)
  MailcapEntry . Entry ct vc <$> fieldList

fieldList :: Parser [Field]
fieldList = field `sepBy` char8 ';'

typefield :: Parser ContentType
typefield = parseContentType

mtext :: Parser B.ByteString
mtext = many' mchar <&> B.pack

mchar :: Parser Word8
mchar = qchar <|> schar

schar :: Parser Word8
schar = satisfy notMChar
  where
    notMChar :: Word8 -> Bool
    notMChar = not . isMChar

isMChar :: Word8 -> Bool
isMChar c = c == 59 -- ';'
            || c == 92 -- '\'
            || c == 10

-- any ASCII control character
isCTLS :: Word8 -> Bool
isCTLS c = c >= 0 && c <= 31 || c == 127

skipSpaceOrQChar :: Parser ()
skipSpaceOrQChar = skipMany ((space $> ()) <|> (qchar $> ()))

field :: Parser Field
field = skipSpaceOrQChar *> (flag <|> namedfield)

namedfield :: Parser Field
namedfield = compose
             <|> composetyped
             <|> print
             <|> test
             <|> x11bitmap
             <|> description
             <|> edit
             <|> textualnewlines

description :: Parser Field
description = stringCI "description" *> equal *> mtext <&> Description

compose :: Parser Field
compose = stringCI "compose" *> equal *> mtext <&> Compose

composetyped :: Parser Field
composetyped = stringCI "composetyped" *> equal *> mtext <&> ComposeTyped

print :: Parser Field
print = stringCI "print" *> equal *> mtext <&> Print

textualnewlines :: Parser Field
textualnewlines = stringCI "textualnewlines" *> equal *> truthy <&> TextualNewlines

truthy :: Parser Bool
truthy = (stringCI "True" $> True)
         <|> stringCI "False" $> False
         <|> (satisfy isTruthy $> True)
         <|> mtext $> False
  where
    isTruthy :: Word8 -> Bool
    isTruthy c = c == 49

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
comment = emptyline <|> prefixedcomment

emptyline :: Parser MailcapLine
emptyline = endOfLine $> Comment "\n"

prefixedcomment :: Parser MailcapLine
prefixedcomment = do
  string "#"
  c <- commenttext
  skipSpace
  pure $ Comment c
  where
    commenttext :: Parser B.ByteString
    commenttext = many' commentchar <&> B.pack
    commentchar = qchar <|> notEOL
    notEOL = satisfy (not . isEndOfLine)

-- | Parsing Help
equal :: Parser ()
equal = skipSpace *> char8 '=' *> skipSpace $> ()

skipQuote :: Parser ()
skipQuote = skip (== 34)

semicolon :: Parser ()
semicolon = char8 ';' $> ()
