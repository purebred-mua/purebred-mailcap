{-# LANGUAGE OverloadedStrings #-}
-- This file is part of purebred-mailcap
-- Copyright (C) 2021 Róman Joost
--
-- purebred-email is free software: you can redistribute it and/or modify
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
import Test.Tasty
import Test.Tasty.HUnit
import Data.Attoparsec.ByteString (parseOnly)

import Data.Mailcap (comment, mailcapentry, MailcapLine(..), Entry(..), Field(..))

main :: IO ()
main =
  defaultMain $ testGroup "Tests"
    [ tests ]

tests :: TestTree
tests = testGroup "Parser tests"
  [ testCommentParsing
  , testEntryParsing
  ]

testCommentParsing :: TestTree
testCommentParsing = testGroup "Mailcap Comment tests"
  [ testCase "empty comment" $
      parseOnly comment "# \n"
      @?= Right (Comment "")
  , testCase "multiple comments" $
      parseOnly comment "# This is a comment \n"
      @?= Right (Comment "This is a comment ")
  ]

testEntryParsing :: TestTree
testEntryParsing = testGroup "Mailcap Entry tests"
  [ testCase "mandatory fields only" $
      parseOnly mailcapentry "application/octet-stream; hexdump; needsterminal; copiousoutput; x11-bitmap=\"/usr/lib/zmail\"\n"
      @?= Right (MailcapEntry $ Entry { _contentType = "application/octet-stream", _viewCommand = "hexdump", _fields= [NeedsTerminal, CopiousOutput, X11Bitmap "/usr/lib/zmail"]})
  ]
