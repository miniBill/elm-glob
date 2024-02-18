# `miniBill/elm-glob` [![Build Status](https://github.com/miniBill/elm-glob/workflows/CI/badge.svg)](https://github.com/miniBill/elm-glob/actions?query=branch%3Amain)

Matches inputs against a [glob](https://en.wikipedia.org/wiki/Glob_%28programming%29).

Usage:

```elm
import Glob

Glob.match "src/*.css" "src/style.css" --> True
Glob.match "src/file.?sv" "src/file.sv" --> False
```
