{ name = "halogen-project"
, dependencies =
  [ "arrays", "console", "effect", "halogen", "maybe", "prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
