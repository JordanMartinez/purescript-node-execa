let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.7-20230204/packages.dhall
        sha256:7364c3a98d654844f0256256f6cc22a03f6a57f571cb757b452aeea5489695fe

in  upstream
      with node-event-emitter.version = "v3.0.0"
      with node-event-emitter.dependencies = 
        [ "effect"
        , "either"
        , "functions"
        , "maybe"
        , "nullable"
        , "prelude"
        , "unsafe-coerce"
        ]
      with node-buffer.version = "v9.0.0"
      with node-buffer.dependencies =
        [ "arraybuffer-types"
        , "effect"
        , "maybe"
        , "st"
        , "unsafe-coerce"
        , "nullable"
        ]
      with node-streams.version = "v9.0.0"
      with node-streams.dependencies =
        [ "aff"
        , "effect"
        , "exceptions"
        , "maybe"
        , "node-buffer"
        , "node-event-emitter"
        , "nullable"
        , "prelude"
        , "unsafe-coerce"
        ]
      with node-fs.version = "v9.1.0"
      with node-fs.dependencies =
        [ "datetime"
        , "effect"
        , "either"
        , "enums"
        , "exceptions"
        , "functions"
        , "integers"
        , "js-date"
        , "maybe"
        , "node-buffer"
        , "node-path"
        , "node-streams"
        , "nullable"
        , "partial"
        , "prelude"
        , "strings"
        , "unsafe-coerce"
        ]
      with node-process.version = "v11.2.0"
      with node-process.dependencies =
        [ "effect"
        , "foreign-object"
        , "foreign"
        , "maybe"
        , "node-event-emitter"
        , "node-streams"
        , "posix-types"
        , "prelude"
        , "unsafe-coerce"
        ]
      with node-child-process.version = "v11.1.0"
      with node-child-process.dependencies =
        [ "exceptions"
        , "node-event-emitter"
        , "foreign"
        , "foreign-object"
        , "functions"
        , "node-fs"
        , "node-streams"
        , "node-os"
        , "nullable"
        , "posix-types"
        , "unsafe-coerce"
        ]
      with node-os =
        { dependencies =
            [ "arrays"
            , "bifunctors"
            , "console"
            , "control"
            , "datetime"
            , "effect"
            , "either"
            , "exceptions"
            , "foldable-traversable"
            , "foreign"
            , "foreign-object"
            , "functions"
            , "maybe"
            , "node-buffer"
            , "nullable"
            , "partial"
            , "posix-types"
            , "prelude"
            , "unsafe-coerce"
            ]
        , repo = "https://github.com/purescript-node/purescript-node-os.git"
        , version = "v5.1.0"
        }
