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
import Data.Mailcap
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
      testEntryParsing,
      testExecutableCommandParsing
    ]

testExecutableCommandParsing :: TestTree
testExecutableCommandParsing =
  testGroup
    "parsing of executable commands"
    [ testCase "simple command" $
        parseOnly viewCommand "xpaint"
          @?= Right (ShellCommand [Argument "xpaint"]),
      testCase "command with path" $
        parseOnly viewCommand "/usr/bin/xpaint"
          @?= Right (ShellCommand [Argument "/usr/bin/xpaint"]),
      testCase "command with mailbody template" $
        parseOnly viewCommand "/usr/bin/xpaint %s"
          @?= Right (ShellCommand [Argument "/usr/bin/xpaint", MailbodyPathTemplate]),
      testCase "shellpipe" $
        parseOnly viewCommand "xwd - frame | foo | bar %s"
          @?= Right (ShellCommand [Argument "xwd", MailbodyPathTemplate])
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
          @?= Right "rplay %s; exit 1",
      testCase "mtext with newline" $
        parseOnly mtext "rplay %s\n"
          @?= Right "rplay %s"
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
                    _viewCommand = ShellCommand [Argument "hexdump"],
                    _fields = []
                  }
            ),
      testCase "flags and named fields (case insensitive)" $
        parseOnly mailcapentry "application/octet-stream; hexdump; neEdstErmInal; copiOusoUtput; x11-BItmap=\"/usr/lib/zmail\"\n"
          @?= Right
            ( MailcapEntry $
                Entry
                  { _contentType = "application/octet-stream",
                    _viewCommand = ShellCommand [Argument "hexdump"],
                    _fields =
                      [ Flag "needsterminal",
                        Flag "copiousoutput",
                        X11Bitmap "\"/usr/lib/zmail\""
                      ]
                  }
            ),
      testCase "wildcard content type" $
        parseOnly mailcapentry "audio/*; rplay %s\\; exit 1\n"
          @?= Right
            ( MailcapEntry $
                Entry
                  { _contentType = "audio/*",
                    _viewCommand = ShellCommand [Argument "rplay %s; exit 1"],
                    _fields = []
                  }
            ),
      testCase "named fields" $
        parseOnly mailcapentry "audio/x-pn-mp3;     realplayer %s; test=test \"$DISPLAY\" != \"\""
          @?= Right
            ( MailcapEntry $
                Entry
                  { _contentType = "audio/x-pn-mp3",
                    _viewCommand = ShellCommand [Argument "realplayer", MailbodyPathTemplate],
                    _fields = [Test "test \"$DISPLAY\" != \"\""]
                  }
            ),
      testCase "multiline entry" $
        parseOnly mailcapentry "audio/basic; showaudio %s; compose=audiocompose %s; \\\n\tedit=audiocompose %s; description=\"An audio fragment\""
          @?= Right
            ( MailcapEntry $
                Entry
                  { _contentType = "audio/basic",
                    _viewCommand = ShellCommand [Argument "showaudio", MailbodyPathTemplate],
                    _fields =
                      [ Compose "audiocompose %s",
                        Edit "audiocompose %s",
                        Description "\"An audio fragment\""
                      ]
                  }
            ),
      testCase "print field" $
        parseOnly mailcapentry "image/x-fax-g3; true; print=printfax %s"
          @?= Right
            ( MailcapEntry $
                Entry
                  { _contentType = "image/x-fax-g3",
                    _viewCommand = ShellCommand [Argument "true"],
                    _fields = [Print "printfax %s"]
                  }
            ),
      testCase "textual new lines - truthy" $
        parseOnly mailcapentry "application/x-backup; /usr/bin/backup %s; textualnewlines=1; test=test -n \"$DISPLAY\""
          @?= Right
            ( MailcapEntry $
                Entry
                  { _contentType = "application/x-backup",
                    _viewCommand = ShellCommand [Argument "/usr/bin/backup", MailbodyPathTemplate],
                    _fields = [TextualNewlines True, Test "test -n \"$DISPLAY\""]
                  }
            ),
      testCase "textual new lines - falsy" $
        parseOnly mailcapentry "application/x-backup; /usr/bin/backup %s; textualnewlines=0; test=test -n \"$DISPLAY\""
          @?= Right
            ( MailcapEntry $
                Entry
                  { _contentType = "application/x-backup",
                    _viewCommand = ShellCommand [Argument "/usr/bin/backup", MailbodyPathTemplate],
                    _fields = [TextualNewlines False, Test "test -n \"$DISPLAY\""]
                  }
            ),
      testCase "composetyped field" $
        parseOnly mailcapentry "message/external-body; showexternal %s %{access-type} %{name} %{site} \\\n\t%{directory} %{mode} %{server}; \\\n\tneedsterminal; composetyped = extcompose %s; \\\n\tdescription=\"A reference to data stored in an external location\""
          @?= Right
            ( MailcapEntry $
                Entry
                  { _contentType = "message/external-body",
                    _viewCommand = ShellCommand [Argument "showexternal %s %{access-type} %{name} %{site} \n\t%{directory} %{mode} %{server}"],
                    _fields =
                      [ Flag "needsterminal",
                        ComposeTyped "extcompose %s",
                        Description "\"A reference to data stored in an external location\""
                      ]
                  }
            )
    ]
