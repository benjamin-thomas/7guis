{ name = "halogen-project"
, dependencies =
  [ "console"
  , "effect"
  , "either"
  , "formatters"
  , "halogen"
  , "maybe"
  , "numbers"
  , "parsing"
  , "prelude"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
