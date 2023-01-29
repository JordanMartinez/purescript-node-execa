let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.7-20230118/packages.dhall
        sha256:973809c5d08a285ac10c6e66be04ca2c85a2d26493644e9196a6a411359f84b9

in  upstream
      with node-event-emitter =
        { dependencies =
          [ "effect"
          , "functions"
          , "prelude"
          , "safe-coerce"
          , "unsafe-coerce"
          ]
          , repo = "https://github.com/JordanMartinez/purescript-node-event-emitter.git"
          , version = "v1.0.1"
          }
      with node-human-signals =
        { dependencies = 
            [ "arrays"
            , "control"
            , "foreign-object"
            , "maybe"
            , "ordered-collections"
            , "prelude"
            ]
        , repo = "https://github.com/JordanMartinez/purescript-node-human-signals.git"
        , version = "v1.0.0"
        }
