{ name = "halogen-project"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "halogen"
  , "halogen-svg-elems"
  , "integers"
  , "numbers"
  , "prelude"
  , "tuples"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
