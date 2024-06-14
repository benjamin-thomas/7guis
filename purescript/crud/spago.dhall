{ name = "halogen-project"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "foldable-traversable"
  , "halogen"
  , "maybe"
  , "prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
