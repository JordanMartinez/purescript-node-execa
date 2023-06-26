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
