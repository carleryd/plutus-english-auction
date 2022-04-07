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
  , "aff-promise"
  , "affjax"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arraybuffer"
  , "arraybuffer-types"
  , "arrays"
  , "console"
  , "effect"
  , "either"
  , "halogen"
  , "halogen-subscriptions"
  , "http-methods"
  , "integers"
  , "maybe"
  , "node-buffer"
  , "numbers"
  , "prelude"
  , "psci-support"
  , "random"
  , "strings"
  , "tailrec"
  , "undefinable"
  , "web-common"
  , "web-dom"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
