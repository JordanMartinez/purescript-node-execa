-- A majority of the below code was ported from this JavaScript library
-- https://github.com/kevva/shebang-command
-- Copyright `shebang-command` contributors
-- MIT License: https://opensource.org/license/mit/
module Node.Library.Execa.ShebangCommand where

import Prelude

import Control.Alternative (guard)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Regex (Regex, match)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)

shebangCommand :: String -> Maybe String
shebangCommand firstLineOfFile = do
  regexMatch <- match shebangRegex firstLineOfFile
  everythingAfterShebang <- join $ Array.index (NEA.toArray regexMatch) 1
  let parts = String.split (String.Pattern " ") everythingAfterShebang

  case Array.uncons parts of
    Just { head: pathOnly, tail: [] } -> do
      binary <- extractBinary pathOnly
      binary <$ guard (binary /= "env")
    Just { head: path, tail: args } -> do
      binary <- extractBinary path
      pure
        if binary == "env" then
          Array.intercalate " " args
        else
          Array.intercalate " " $ Array.cons binary args
    _ -> Nothing
  where
  extractBinary = Array.last <<< String.split (String.Pattern "/")

  shebangRegex :: Regex
  shebangRegex = unsafeRegex """^#! ?(.*)""" noFlags
