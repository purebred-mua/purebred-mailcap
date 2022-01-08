module Data.RFC1524.Internal
  ( ContentType (..),
    parseContentType,
    ci,
    token,
    niceEndOfInput,
  )
where

import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.Internal.Types as AT
import Data.Attoparsec.ByteString.Char8 (char8, peekChar')
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.CaseInsensitive as CI
import Control.Applicative ((<|>))
import Data.String (IsString (fromString))

-- borrowed from purebred-email
data ContentType = ContentType (CI.CI B.ByteString) (CI.CI B.ByteString)
  deriving (Show, Eq)

instance IsString ContentType where
  fromString = either err id . parseOnly parseContentType . C8.pack
    where
      err msg = error $ "failed to parse Content-Type: " <> msg

parseContentType :: Parser ContentType
parseContentType = do
  typ <- ci token
  _ <- char8 '/'
  subtype <- ci token
  pure $ ContentType typ subtype

-- | Modify a parser to produce a case-insensitive value
ci :: CI.FoldCase s => Parser s -> Parser (CI.CI s)
ci = fmap CI.mk

-- | header token parser
token :: Parser B.ByteString
token =
  takeWhile1 (\c -> c >= 33 && c <= 126 && notInClass "()<>@,;:\\\"/[]?=" c)

-- end --

niceEndOfInput :: Parser ()
niceEndOfInput = endOfInput <|> p
  where
    p = do
      c <- peekChar'
      end <- takeByteString
      off <- offset
      fail $ "unexpected " <> show c <> " at offset " <> show off <> "remaining: " <> show end

offset :: AT.Parser i Int
offset = AT.Parser $ \t pos more _lose suc -> suc t pos more (AT.fromPos pos)
