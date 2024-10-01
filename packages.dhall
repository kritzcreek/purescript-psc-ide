let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.14-20240227/packages.dhall sha256:c9633eb78193aac138d7debbc907bfedb8f2e3025ef5a874b8dbc1f35b75eef4

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions
