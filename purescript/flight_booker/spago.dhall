{ name = "halogen-project"
, dependencies =
  [ "arrays"
  , "bifunctors"
  , "console"
  , "datetime"
  , "effect"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "free"
  , "halogen"
  , "integers"
  , "maybe"
  , "parsing"
  , "partial"
  , "prelude"
  , "strings"
  , "test-unit"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
