module Data.Mailcap.Internal (
  ContentType(..)
  , parseContentType
  , ci
  , token
  , qchar) where

import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (char8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.CaseInsensitive as CI
import Data.Word (Word8)
import Data.String (IsString(..))

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

qchar :: Parser Word8
qchar = char8 '\\' *> anyWord8