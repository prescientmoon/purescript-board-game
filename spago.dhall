{ name = "board-game-thingy"
, dependencies =
  [ "aff"
  , "arrays"
  , "control"
  , "datetime"
  , "effect"
  , "foldable-traversable"
  , "halogen"
  , "halogen-subscriptions"
  , "halogen-svg-elems"
  , "integers"
  , "math"
  , "maybe"
  , "media-types"
  , "newtype"
  , "prelude"
  , "profunctor-lenses"
  , "psci-support"
  , "sized-vectors"
  , "strings"
  , "tailrec"
  , "tuples"
  , "typelevel"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
