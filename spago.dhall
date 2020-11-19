{ name = "board-game-thingy"
, dependencies =
  [ "prelude"
  , "debug"
  , "debugged"
  , "generics-rep"
  , "psci-support"
  , "effect"
  , "console"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
