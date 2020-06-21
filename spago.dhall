{ name = "psc-ide"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "effect"
  , "maybe"
  , "node-child-process"
  , "node-fs"
  , "parallel"
  , "random"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
