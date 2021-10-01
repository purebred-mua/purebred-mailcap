{-# LANGUAGE OverloadedStrings #-}
module Data.Mailcap () where

import Control.Applicative ((<|>))
import qualified Data.Text as T
import Data.Attoparsec.Text (Parser, many', string, isEndOfLine, takeTill, anyChar, sepBy, char)

type MailcapFile = [MailcapLine]

data MailcapLine = Comment T.Text
                 | MailcapEntry ContentType ViewCommand [Field]

type Comment = T.Text
type ContentType = T.Text
type ViewCommand = T.Text
type Field = T.Text


mailcapfile :: Parser MailcapFile
mailcapfile = many' mailcapline

mailcapline :: Parser MailcapLine
mailcapline = mailcapentry <|> comment

mailcapentry :: Parser MailcapLine
mailcapentry = do
  ct <- takeTill (== ';')
  vc <- takeTill (== ';')
  fields <- takeTill (== ';') `sepBy` char ';'
  pure $ MailcapEntry ct vc fields
  
comment :: Parser MailcapLine
comment = do
  txt <- "#" *> takeTill isEndOfLine 
  pure $ Comment txt
