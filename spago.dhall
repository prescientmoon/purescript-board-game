{ name = "board-game-thingy"
, dependencies =
  [ "console"
  , "debug"
  , "debugged"
  , "effect"
  , "generics-rep"
  , "halogen"
  , "halogen-svg-elems"
  , "prelude"
  , "profunctor-lenses"
  , "psci-support"
  , "sized-vectors"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
