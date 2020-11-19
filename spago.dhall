{ name = "board-game-thingy"
, dependencies =
  [ "console"
  , "debug"
  , "debugged"
  , "effect"
  , "generics-rep"
  , "halogen"
  , "prelude"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
