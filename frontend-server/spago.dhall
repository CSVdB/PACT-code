{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "affjax"
  , "argonaut-core"
  , "arrays"
  , "bifunctors"
  , "codec"
  , "codec-argonaut"
  , "console"
  , "const"
  , "datetime"
  , "dom-indexed"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "formatters"
  , "halogen"
  , "halogen-formless"
  , "halogen-store"
  , "http-methods"
  , "maybe"
  , "naturals"
  , "newtype"
  , "now"
  , "prelude"
  , "profunctor"
  , "psci-support"
  , "routing"
  , "routing-duplex"
  , "safe-coerce"
  , "strings"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "variant"
  , "web-events"
  , "web-html"
  , "web-storage"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
