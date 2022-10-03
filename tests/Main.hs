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

import ViewCommand

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [
        tests
      , ViewCommand.testExecutableCommandParsing
      ]

tests :: TestTree
tests =
  testGroup
    "Parser tests"
    [ testCommentParsing
    , testMailcapfileParsing
    , testFieldParsing
    , testEntryParsing
    ]

testMailcapfileParsing :: TestTree
testMailcapfileParsing =
  testGroup
    "Mailcap field tests"
    [ testCase "multiple comments" $
        parseOnly mailcapfile "# This is a comment \n# Another\n# comment"
          @?= Right [Comment " This is a comment ", Comment " Another", Comment " comment"]
    , testCase "whitespace prefixed" $
        parseOnly mailcapfile "\n\n# comment\naudio/*; rplay %s\n"
          @?= Right [ Comment "\n"
                    , Comment "\n"
                    , Comment " comment"
                    , MailcapEntry $
                      Entry { _contentType = "audio/*"
                            , _viewCommand = ShellCommand [
                                  Argument "rplay"
                                , MailbodyPathTemplate]
                            , _fields = []
                            }
                    ]
    ]

testCommentParsing :: TestTree
testCommentParsing =
  testGroup
    "Mailcap field tests"
    [ testCase "empty comment" $
        parseOnly comment "# \n"
          @?= Right (Comment " "),
      testCase "commented out mailcap lines" $
        parseOnly comment "#audio/basic; showaudio %s; compose=audiocompose %s;\\\n#\tedit=audiocompose %s; description=\"An audio fragment\"\n"
          @?= Right (Comment "audio/basic; showaudio %s; compose=audiocompose %s;\n#\tedit=audiocompose %s; description=\"An audio fragment\"")
    ]

testFieldParsing :: TestTree
testFieldParsing =
  testGroup
    "Mailcap field tests"
    [ testCase "mtext" $
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
        parseOnly mailcapentry "audio/*; rplay %s \\; exit 1\n"
          @?= Right
            ( MailcapEntry $
                Entry
                  { _contentType = "audio/*",
                    _viewCommand = ShellCommand [Argument "rplay", MailbodyPathTemplate, Argument ";", Argument "exit", Argument "1"],
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
                    _viewCommand = ShellCommand [ Argument "showexternal"
                                                , MailbodyPathTemplate
                                                , Argument "%{access-type}"
                                                , Argument "%{name}"
                                                , Argument "%{site}"
                                                , Argument "\n"
                                                , Argument "%{directory}"
                                                , Argument "%{mode}"
                                                , Argument "%{server}"
                                                ],
                    _fields =
                      [ Flag "needsterminal",
                        ComposeTyped "extcompose %s",
                        Description "\"A reference to data stored in an external location\""
                      ]
                  }
            )
    ]
