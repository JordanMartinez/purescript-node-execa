module Test.Node.Library.ShebangCommand where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Node.Library.Execa.ShebangCommand (shebangCommand)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "shebangCommand" do
    let
      tests = do
        space <- [ true, false ]
        wrapEnv <- [ true, false ]
        argCount <- [ 0, 1, 2 ]
        pure { space, wrapEnv, argCount }

    for_ tests \r -> do
      let
        expected =
          Array.intercalate " "
            $ Array.cons "foo"
            $ Array.replicate r.argCount "a"
        cmd =
          append "#!"
            $ append (guard r.space " ")
            $ append (if r.wrapEnv then "/usr/bin/env " else "/")
            $ expected
      it ("should parse `" <> cmd <> "` as `" <> expected <> "`") do
        shebangCommand cmd `shouldEqual` (Just expected)

