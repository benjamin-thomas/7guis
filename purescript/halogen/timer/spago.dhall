{ name = "halogen-project"
, dependencies =
  [ "aff"
  , "console"
  , "datetime"
  , "effect"
  , "halogen"
  , "halogen-subscriptions"
  , "maybe"
  , "now"
  , "numbers"
  , "prelude"
  , "tailrec"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
