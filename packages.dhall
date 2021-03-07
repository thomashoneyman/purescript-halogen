let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0/packages.dhall sha256:710b53c085a18aa1263474659daa0ae15b7a4f453158c4f60ab448a6b3ed494e

in  upstream
  with event.version = "master"
  with event.repo = "https://github.com/thomashoneyman/purescript-event"
  with event.dependencies
       =
    [ "prelude"
    , "console"
    , "effect"
    , "filterable"
    , "nullable"
    , "unsafe-reference"
    , "js-timers"
    , "now"
    ]
  with filterable.version = "master"
  with filterable.repo
       = "https://github.com/thomashoneyman/purescript-filterable"
