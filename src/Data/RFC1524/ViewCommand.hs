{-# LANGUAGE OverloadedStrings #-}
module Data.RFC1524.ViewCommand (
    viewCommand
  , shellargument
  , argument
  , ShellArgument (..)
  , ExecutableCommand (..)
  ) where

import Data.Functor (($>), (<&>))
import Data.Attoparsec.ByteString.Char8 (isSpace_w8)
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString
import Data.Word (Word8)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as B

import Data.RFC1524.Internal (qchar)

-- | Parsing of executable commands
data ShellArgument
  = Argument String -- xwd -frame | foo | bar
  | MailbodyPathTemplate -- %s
  | ContentTypeTemplate -- %t e.g. text/plain
  | NamedContentTypeParameter String -- 42 from boundary=42
  | FChar B.ByteString
  deriving (Show, Eq)

newtype ExecutableCommand
  = ShellCommand [ShellArgument]
  deriving (Show, Eq)

viewCommand :: Parser ExecutableCommand
viewCommand = do
  args <- shellargument `sepBy` whitespace
  pure $ ShellCommand args

shellargument :: Parser ShellArgument
shellargument =
  (string "%s" $> MailbodyPathTemplate)
    <|> (string "%t" $> ContentTypeTemplate)
    <|> argument
    <|> (takeTill isSpace_w8 <&> FChar)

-- parse an argument of a command line typically just the command line
-- path or a shell pipe
-- e.g. xwd - frame | foo | bar
argument :: Parser ShellArgument
argument = Argument . C8.unpack <$> consume
  where
    consume :: Parser B.ByteString
    consume = many' anyChar' <&> B.pack

-- either quoted char or anything not a semicolon, whitespace or newline
anyChar' :: Parser Word8
anyChar' = qchar <|> satisfy (\w -> not $ isSemicolon w || isWhitespace w || isNewLine w)

-- space and horizontal tab
isWhitespace :: Word8 -> Bool
isWhitespace c = c == 32 || c == 9

isNewLine :: Word8 -> Bool
isNewLine c = c == 10

isSemicolon :: Word8 -> Bool
isSemicolon c = c == 59

-- | parse only Space and horizontal tab
whitespace :: Parser Word8
whitespace = satisfy isWhitespace
