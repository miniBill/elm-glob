module Examples exposing (matchTests, parseTests)

import Test exposing (Test, describe)
import Utils


expectations : List Utils.Expectation
expectations =
    [ { glob = "src/foo.css", input = "src/foo.css", expected = True }
    , { glob = "src/other.css", input = "src/foo.css", expected = False }
    , { glob = "src/*.css", input = "src/foo.css", expected = True }
    , { glob = "src/*.css", input = "src/foo/bar.css", expected = False }
    , { glob = "src/fo*.css", input = "src/foo.css", expected = True }
    , { glob = "src/foo*.css", input = "src/foo.css", expected = True }
    , { glob = "src/**/*.css", input = "src/foo/bar.css", expected = True }
    , { glob = "src/**/*.css", input = "src/foo/bar/baz.css", expected = True }
    , { glob = "src/**/*.css", input = "src/foo.css", expected = True }
    , { glob = "src/*.css", input = "src/foo.pcss", expected = False }
    , { glob = "src/*.css", input = "src/foo.cssZ", expected = False }
    , { glob = "src/*.css", input = "other/src/foo.css", expected = False }
    , { glob = "src/*.{css,html}", input = "src/foo.css", expected = True }
    , { glob = "src/*.{css,html}", input = "src/foo.html", expected = True }
    , { glob = "src/*.{css,html}", input = "src/foo.js", expected = False }
    , { glob = "src/foo.?ss", input = "src/foo.css", expected = True }
    , { glob = "src/foo.?ss", input = "src/foo.ss", expected = False }
    , { glob = "**/*", input = "src/foo.css", expected = True }
    , { glob = "src/foo", input = "src/foo/bar.css", expected = False }
    , { glob = "weird\\/name", input = "weird\\/name", expected = True }
    , { glob = "weird\\/name", input = "weird/name", expected = False }
    , { glob = "src/\\*.css", input = "src/*.css", expected = True }
    , { glob = "src/\\*.css", input = "src/a.css", expected = False }
    , { glob = "[^]", input = "^", expected = True }

    -- , { glob = "src/*.{css\\,html}", input = "src/foo.css,html", expected = True }
    -- , { glob = "src/*.{css\\,html}", input = "src/foo.css", expected = False }
    -- , { glob = "src/*.{css\\,html}", input = "src/foo.html", expected = False }
    ]


parseTests : Test
parseTests =
    expectations
        |> Utils.parseAll
        |> describe "Glob.fromString"


matchTests : Test
matchTests =
    expectations
        |> Utils.checkAll
        |> describe "Glob.match"
