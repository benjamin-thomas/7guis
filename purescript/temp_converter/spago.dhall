{ name = "halogen-project"
, dependencies =
  [ "console"
  , "effect"
  , "either"
  , "halogen"
  , "maybe"
  , "numbers"
  , "prelude"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
