module Test.Node.Library.ParseCommand where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Node.Library.Execa.ParseCommand (parseCommand')
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "parseCommand" do
    let
      shouldBeFileArgs file args = do
        let result = parseCommand' $ (fst file) <> " " <> (Array.intercalate " " $ map fst args)
        Right (snd file) `shouldEqual` (map _.file) result
        Right (map snd args) `shouldEqual` (map _.args) result

      escapeSlash = """\"""
      backslash = escapeSlash
      space = " "
      escapedSpace = escapeSlash <> " "
      dquote = "\""
      squote = "'"
      escSQuote = escapeSlash <> squote
      escDQuote = escapeSlash <> dquote
      escBackslash = escapeSlash <> backslash

    it "should work despite extra spaces" do
      shouldBeFileArgs
        (" file  " /\ "file")
        [ "    arg1" /\ "arg1"
        , "arg2   " /\ "arg2"
        , "   arg3   " /\ "arg3"
        ]

    it "should account for escaped spaces, double-quotes, single-quotes, and back slashes" do
      shouldBeFileArgs
        ("file" /\ "file")
        [ ("a" <> escapedSpace <> "b") /\ ("a" <> space <> "b")
        , ("a" <> escDQuote <> "b") /\ ("a" <> dquote <> "b")
        , ("a" <> escSQuote <> "b") /\ ("a" <> squote <> "b")
        , ("a" <> escBackslash <> "b") /\ ("a" <> backslash <> "b")
        ]

    it "should account for escaped double-quotes within double-quote context" do
      shouldBeFileArgs
        ("file" /\ "file")
        [ (dquote <> "a" <> escDQuote <> "b" <> dquote) /\ ("a" <> dquote <> "b")
        ]

    it "should account for escaped single-quotes within single-quote context" do
      shouldBeFileArgs
        ("file" /\ "file")
        [ (squote <> "a" <> escSQuote <> "b" <> squote) /\ ("a" <> squote <> "b")
        ]

    it "should account for ignore all other escaped chars in a double- or single-quote context" do
      shouldBeFileArgs
        ("file" /\ "file")
        [ (dquote <> "a" <> escSQuote <> "b" <> dquote) /\ ("a" <> escSQuote <> "b")
        , (squote <> "a" <> escDQuote <> "b" <> squote) /\ ("a" <> escDQuote <> "b")
        ]

