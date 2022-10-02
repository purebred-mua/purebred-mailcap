{-# LANGUAGE OverloadedStrings #-}
module ViewCommand where

import Data.RFC1524.ViewCommand

import Data.Attoparsec.ByteString (parseOnly)
import Test.Tasty
import Test.Tasty.HUnit

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
          @?= Right (ShellCommand [Argument "xwd", Argument "-", Argument "frame", Argument "|", Argument "foo", Argument "|", Argument "bar",  MailbodyPathTemplate]),
      -- TODO: won't parse rplay %s\\; exit 1
      testCase "quoted characters" $
        parseOnly viewCommand "rplay %s \\; exit 1"
          @?= Right (ShellCommand [Argument "rplay",  MailbodyPathTemplate, Argument ";", Argument "exit", Argument "1"])
    ]
