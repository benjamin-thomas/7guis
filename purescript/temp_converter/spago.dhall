{ name = "halogen-project"
, dependencies =
  [ "console"
  , "effect"
  , "either"
  , "formatters"
  , "halogen"
  , "maybe"
  , "parsing"
  , "prelude"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
