-- This file is part of purebred-mailcap
-- Copyright (C) 2022 Róman Joost
--
-- purebred-mailcap is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
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
