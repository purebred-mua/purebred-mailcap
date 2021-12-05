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

import Data.Attoparsec.ByteString (parseOnly)
import Data.Mailcap (Entry (..), Field (..), MailcapLine (..), comment, mailcapentry)
import Data.RFC1524 (mtext)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [tests]

tests :: TestTree
tests =
  testGroup
    "Parser tests"
    [ testFieldParsing,
      testEntryParsing
    ]

testFieldParsing :: TestTree
testFieldParsing =
  testGroup
    "Mailcap field tests"
    [ testCase "empty comment" $
        parseOnly comment "# \n"
          @?= Right (Comment ""),
      testCase "multiple comments" $
        parseOnly comment "# This is a comment \n"
          @?= Right (Comment "This is a comment "),
      testCase "mtext" $
        parseOnly mtext "rplay %s\\; exit 1"
          @?= Right "rplay %s; exit 1"
    ]

testEntryParsing :: TestTree
testEntryParsing =
  testGroup
    "Mailcap Entry tests"
    [ testCase "mandatory fields only" $
        parseOnly mailcapentry "application/octet-stream; hexdump\n"
          @?= Right
            ( MailcapEntry $
                Entry
                  { _contentType = "application/octet-stream",
                    _viewCommand = "hexdump\n",
                    _fields = []
                  }
            ),
      testCase "flags and named fields (case insensitive)" $
        parseOnly mailcapentry "application/octet-stream; hexdump; neEdstErmInal; copiOusoUtput; x11-BItmap=\"/usr/lib/zmail\"\n"
          @?= Right
            ( MailcapEntry $
                Entry
                  { _contentType = "application/octet-stream",
                    _viewCommand = "hexdump",
                    _fields =
                      [ Flag "needsterminal",
                        Flag "copiousoutput",
                        X11Bitmap "\"/usr/lib/zmail\"\n"
                      ]
                  }
            ),
      testCase "wildcard content type" $
        parseOnly mailcapentry "audio/*; rplay %s\\; exit 1\n"
          @?= Right
            ( MailcapEntry $
                Entry
                  { _contentType = "audio/*",
                    _viewCommand = "rplay %s; exit 1\n",
                    _fields = []
                  }
            ),
      testCase "named fields" $
        parseOnly mailcapentry "audio/x-pn-mp3;     realplayer %s; test=test \"$DISPLAY\" != \"\""
          @?= Right
            ( MailcapEntry $
                Entry
                  { _contentType = "audio/x-pn-mp3",
                    _viewCommand = "realplayer %s",
                    _fields = [Test "test \"$DISPLAY\" != \"\""]
                  }
            ),
      testCase "multiline entry" $
        parseOnly mailcapentry "audio/basic; showaudio %s; compose=audiocompose %s; \\\n\tedit=audiocompose %s; description=\"An audio fragment\""
          @?= Right
            ( MailcapEntry $
                Entry
                  { _contentType = "audio/basic",
                    _viewCommand = "showaudio %s",
                    _fields =
                      [ Compose "audiocompose %s",
                        Edit "audiocompose %s",
                        Description "\"An audio fragment\""
                      ]
                  }
            ),
      testCase "print field" $
        parseOnly mailcapentry "image/x-fax-g3;; print=printfax %s"
          @?= Right
            ( MailcapEntry $
                Entry
                  { _contentType = "image/x-fax-g3",
                    _viewCommand = "",
                    _fields = [Print "printfax %s"]
                  }
            ),
      testCase "composetyped field" $
        parseOnly mailcapentry "message/external-body; showexternal %s %{access-type} %{name} %{site} \\\n\t%{directory} %{mode} %{server}; \\\n\tneedsterminal; composetyped = extcompose %s; \\\n\tdescription=\"A reference to data stored in an external location\""
          @?= Right
            ( MailcapEntry $
                Entry
                  { _contentType = "message/external-body",
                    _viewCommand = "showexternal %s %{access-type} %{name} %{site} \n\t%{directory} %{mode} %{server}",
                    _fields = [Flag "needsterminal",
                               ComposeTyped "extcompose %s",
                               Description "\"A reference to data stored in an external location\""]
                  }
            )
    ]
