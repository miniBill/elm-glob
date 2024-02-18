module Examples exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Glob
import Test exposing (..)


expectations : List { glob : String, input : String, expected : Bool }
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
    ]


all : Test
all =
    List.map
        (\{ glob, input, expected } ->
            test ("Glob: " ++ glob ++ " Input: " ++ input)
                (\() ->
                    -- match to be implemented yourself :D
                    Glob.match glob input
                        |> Expect.equal expected
                )
        )
        expectations
        |> describe "GlobToRegex"
