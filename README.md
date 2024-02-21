# `miniBill/elm-glob` [![Build Status](https://github.com/miniBill/elm-glob/workflows/CI/badge.svg)](https://github.com/miniBill/elm-glob/actions?query=branch%3Amain)

Matches file paths against a [glob](https://man7.org/linux/man-pages/man7/glob.7.html).

```elm
import Glob exposing (Glob)

glob : Glob
glob =
    Glob.fromString "src/*.css*"
        |> Result.withDefault Glob.never

Glob.match glob "src/style.css"
--> True

Glob.match glob "src/file.sv"
--> False
```
