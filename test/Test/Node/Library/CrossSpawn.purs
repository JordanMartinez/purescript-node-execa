module Test.Node.Library.Execa.CrossSpawn where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Node.Library.Execa.CrossSpawn (parse)
import Test.Node.Library.Utils (isWindows)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)

spec :: Spec Unit
spec = do
  describe "CrossSpawn" do
    describe "parse" do
      when isWindows do
        it "should parse a long `purs graph` command" do
          result <- liftEffect $ parse
            pursCmd
            pursGraphCmd
            { shell: Nothing
            , env: Nothing
            , cwd: Nothing
            , windowsEnableCmdEcho: false
            , windowsVerbatimArguments: Nothing
            , windowsPreventCmdShim: true
            }
          when (result.command == pursCmd) do
            fail $ append "\n" $ Array.intercalate "\n"
              $
                [ "Expected absolute path to purs.cmd, not relative one"
                , "Command: " <> result.command
                , "Args: "
                ]
                  <> result.args

pursCmd :: String
pursCmd = "purs.cmd"

pursGraphCmd :: Array String
pursGraphCmd =
  [ "graph"
  , ".spago\\packages\\aff-7.1.0\\src\\**\\*.purs"
  , ".spago\\packages\\aff-promise-4.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\affjax-13.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\affjax-node-1.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\ansi-7.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\argonaut-core-7.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\arraybuffer-types-3.0.2\\src\\**\\*.purs"
  , ".spago\\packages\\arrays-7.3.0\\src\\**\\*.purs"
  , ".spago\\packages\\assert-6.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\avar-5.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\b64-0.0.8\\src\\**\\*.purs"
  , ".spago\\packages\\bifunctors-6.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\catenable-lists-7.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\codec-6.1.0\\src\\**\\*.purs"
  , ".spago\\packages\\codec-argonaut-10.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\colors-7.0.1\\src\\**\\*.purs"
  , ".spago\\packages\\console-6.1.0\\src\\**\\*.purs"
  , ".spago\\packages\\const-6.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\contravariant-6.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\control-6.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\convertable-options-1.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\css-6.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\datetime-6.1.0\\src\\**\\*.purs"
  , ".spago\\packages\\debug-6.0.2\\src\\**\\*.purs"
  , ".spago\\packages\\distributive-6.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\dodo-printer\\v2.2.1\\src\\**\\*.purs"
  , ".spago\\packages\\dom-indexed-12.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\effect-4.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\either-6.1.0\\src\\**\\*.purs"
  , ".spago\\packages\\encoding-0.0.8\\src\\**\\*.purs"
  , ".spago\\packages\\enums-6.0.1\\src\\**\\*.purs"
  , ".spago\\packages\\exceptions-6.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\exists-6.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\exitcodes-4.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\fetch-4.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\filterable-5.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\fixed-points-7.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\foldable-traversable-6.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\foreign-7.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\foreign-object-4.1.0\\src\\**\\*.purs"
  , ".spago\\packages\\fork-6.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\form-urlencoded-7.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\formatters-7.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\free-7.1.0\\src\\**\\*.purs"
  , ".spago\\packages\\freeap-7.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\functions-6.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\functors-5.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\gen-4.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\graphs-8.1.0\\src\\**\\*.purs"
  , ".spago\\packages\\halogen-7.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\halogen-css-10.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\halogen-subscriptions-2.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\halogen-vdom-8.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\html-parser-halogen\\035a51d02ba9f8b70c3ffd9fe31a3f5bed19941c\\src\\**\\*.purs"
  , ".spago\\packages\\http-methods-6.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\identity-6.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\integers-6.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\invariant-6.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\js-date-8.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\js-fetch-0.2.1\\src\\**\\*.purs"
  , ".spago\\packages\\js-promise-1.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\js-promise-aff-1.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\js-timers-6.1.0\\src\\**\\*.purs"
  , ".spago\\packages\\js-uri-3.1.0\\src\\**\\*.purs"
  , ".spago\\packages\\json-codecs-4.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\language-cst-parser\\v0.13.0\\src\\**\\*.purs"
  , ".spago\\packages\\language-purescript\\db4377dea03ba9c5273a93a8368a53f1d87882c1\\src\\**\\*.purs"
  , ".spago\\packages\\lazy-6.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\lcg-4.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\lists-7.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\markdown-it-halogen\\08c9625015bf04214be14e45230e8ce12f3fa2bf\\src\\**\\*.purs"
  , ".spago\\packages\\markdown-it\\f6e8ee91298f2fc13c4277e75a19e0538de5f7a2\\src\\**\\*.purs"
  , ".spago\\packages\\maybe-6.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\media-types-6.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\mmorph-7.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\newtype-5.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\node-buffer-9.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\node-child-process-11.1.0\\src\\**\\*.purs"
  , ".spago\\packages\\node-event-emitter-3.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\node-execa-5.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\node-fs-9.1.0\\src\\**\\*.purs"
  , ".spago\\packages\\node-human-signals-1.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\node-os-5.1.0\\src\\**\\*.purs"
  , ".spago\\packages\\node-path-5.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\node-process-11.2.0\\src\\**\\*.purs"
  , ".spago\\packages\\node-streams-9.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\nonempty-7.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\now-6.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\nullable-6.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\numbers-9.0.1\\src\\**\\*.purs"
  , ".spago\\packages\\open-memoize-6.1.0\\src\\**\\*.purs"
  , ".spago\\packages\\options-7.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\optparse\\a9b227110c34ae4f8c1b2fcb07de6f1327ce5826\\src\\**\\*.purs"
  , ".spago\\packages\\ordered-collections-3.1.1\\src\\**\\*.purs"
  , ".spago\\packages\\orders-6.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\parallel-7.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\parsing-10.2.0\\src\\**\\*.purs"
  , ".spago\\packages\\partial-4.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\pipes-8.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\posix-types-6.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\prelude-6.0.1\\src\\**\\*.purs"
  , ".spago\\packages\\profunctor-6.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\profunctor-lenses-8.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\psci-support-6.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\quickcheck-8.0.1\\src\\**\\*.purs"
  , ".spago\\packages\\quickcheck-laws-7.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\random-6.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\record-4.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\refs-6.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\registry-foreign\\d7d35c94cc286528e506a6a7ca78d22c84b251c9\\foreign\\src\\**\\*.purs"
  , ".spago\\packages\\registry-lib\\d7d35c94cc286528e506a6a7ca78d22c84b251c9\\lib\\src\\**\\*.purs"
  , ".spago\\packages\\routing-duplex-0.7.0\\src\\**\\*.purs"
  , ".spago\\packages\\safe-coerce-2.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\search-trie\\e7f7f22486a1dba22171ec885dbc2149dc815119\\src\\**\\*.purs"
  , ".spago\\packages\\spec-7.5.5\\src\\**\\*.purs"
  , ".spago\\packages\\st-6.2.0\\src\\**\\*.purs"
  , ".spago\\packages\\string-parsers-8.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\strings-6.0.1\\src\\**\\*.purs"
  , ".spago\\packages\\stringutils-0.0.12\\src\\**\\*.purs"
  , ".spago\\packages\\tailrec-6.1.0\\src\\**\\*.purs"
  , ".spago\\packages\\these-6.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\transformers-6.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\tuples-7.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\type-equality-4.0.1\\src\\**\\*.purs"
  , ".spago\\packages\\typelevel-prelude-7.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\unfoldable-6.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\unicode-6.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\unsafe-coerce-6.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\unsafe-reference-5.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\variant-8.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\versions-7.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\web-clipboard-5.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\web-dom-6.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\web-events-4.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\web-file-4.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\web-html-4.1.0\\src\\**\\*.purs"
  , ".spago\\packages\\web-pointerevents-2.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\web-storage-5.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\web-streams-4.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\web-touchevents-4.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\web-uievents-5.0.0\\src\\**\\*.purs"
  , ".spago\\packages\\web-xhr-5.0.1\\src\\**\\*.purs"
  , "D:\\a\\spago\\spago\\.spago\\BuildInfo.purs"
  , "core\\src\\**\\*.purs"
  , "docs-search\\common\\src\\**\\*.purs"
  , "docs-search\\index\\src\\**\\*.purs"
  , "docs-search\\index\\test\\**\\*.purs"
  , "src\\**\\*.purs"
  , "test\\**\\*.purs"
  ]
