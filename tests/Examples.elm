module Examples exposing (matchTests, parseTests)

import Expect
import Glob
import Set
import Test exposing (Test, describe, test)


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
    , { glob = "src/foo", input = "src/foo/bar.css", expected = False }
    , { glob = "weird\\/name", input = "weird\\/name", expected = True }
    , { glob = "weird\\/name", input = "weird/name", expected = False }
    , { glob = "src/\\*.css", input = "src/*.css", expected = True }
    , { glob = "src/\\*.css", input = "src/a.css", expected = False }
    , { glob = "src/*.{css\\,html}", input = "src/foo.css,html", expected = True }
    , { glob = "src/*.{css\\,html}", input = "src/foo.css", expected = False }
    , { glob = "src/*.{css\\,html}", input = "src/foo.html", expected = False }
    ]


parseTests : Test
parseTests =
    expectations
        |> List.map .glob
        |> Set.fromList
        |> Set.toList
        |> List.map
            (\glob ->
                test ("Glob: " ++ glob) <|
                    \_ ->
                        case Glob.parse glob of
                            Err _ ->
                                Expect.fail "Failed to parse"

                            Ok _ ->
                                Expect.pass
            )
        |> describe "Glob.parse"


matchTests : Test
matchTests =
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
        |> describe "Glob.match"
