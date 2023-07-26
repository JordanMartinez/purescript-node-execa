let config = ./spago.dhall
in config
    with sources = config.sources # [ "test/**/*.purs" ]
    with dependencies = config.dependencies #
      [ "spec" 
      , "datetime"
      ]
