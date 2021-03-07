let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210304/packages.dhall sha256:c88151fe7c05f05290224c9c1ae4a22905060424fb01071b691d3fe2e5bad4ca

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
