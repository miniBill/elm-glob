module WcMatch exposing (matchTests, parseTests)

import Test exposing (Test, describe)
import Utils


expectations : List Utils.Expectation
expectations =
    -- These test cases come from https://github.com/facelessuser/wcmatch/blob/main/tests/test_fnmatch.py
    [ { glob = "src/foo.css", input = "src/foo.css", expected = True }

    -- Basic test of traditional features
    , { glob = "abc", input = "abc", expected = True }
    , { glob = "?*?", input = "abc", expected = True }
    , { glob = "???*", input = "abc", expected = True }
    , { glob = "*???", input = "abc", expected = True }
    , { glob = "???", input = "abc", expected = True }
    , { glob = "*", input = "abc", expected = True }
    , { glob = "ab[cd]", input = "abc", expected = True }
    , { glob = "ab[!de]", input = "abc", expected = True }
    , { glob = "ab[de]", input = "abc", expected = False }
    , { glob = "??", input = "a", expected = False }
    , { glob = "b", input = "a", expected = False }

    -- Test that '\' is handled correctly in character sets;
    -- , { glob = "[\\]", input = "\\", expected = False } -- I disagree here
    -- , { glob = "[!\\]", input = "a", expected = False } -- I disagree here
    , { glob = "[!\\]", input = "\\", expected = False }
    , { glob = "[\\\\]", input = "\\", expected = True }
    , { glob = "[!\\\\]", input = "a", expected = True }
    , { glob = "[!\\\\]", input = "\\", expected = False }

    -- Test that filenames with newlines in them are handled correctly.
    , { glob = "foo*", input = "foo\nbar", expected = True }
    , { glob = "foo*", input = "foo\nbar\n", expected = True }

    -- , { glob = "foo*", input = "\nfoo", expected = False } -- Edge case, will possibly handle eventually
    , { glob = "*", input = "\n", expected = True }

    -- Case: General
    , { glob = "abc", input = "abc", expected = True }
    , { glob = "abc", input = "AbC", expected = False }
    , { glob = "AbC", input = "abc", expected = False }
    , { glob = "AbC", input = "AbC", expected = True }

    -- Wildcard tests
    , { glob = "te*", input = "test", expected = True }
    , { glob = "te*ÿ", input = "testÿ", expected = True }
    , { glob = "foo*", input = "foo\nbar", expected = True }

    -- Ensure that we don't fail on regular expression related symbols
    -- such as &&, ||, ~~, --, or [.  Currently re doesn't do anything with
    -- && etc., but they are handled special in re as there are plans to utilize them.
    , { glob = "[[]", input = "[", expected = True }
    , { glob = "[a&&b]", input = "&", expected = True }
    , { glob = "[a||b]", input = "|", expected = True }
    , { glob = "[a~~b]", input = "~", expected = True }
    , { glob = "[a-z+--A-Z]", input = ",", expected = True }
    , { glob = "[a-z--/A-Z]", input = ".", expected = True }

    -- -- `Dotmatch` cases
    , { glob = ".abc", input = ".abc", expected = True }
    , { glob = "\\.abc", input = ".abc", expected = True }
    , { glob = "?abc", input = ".abc", expected = True }
    , { glob = "*abc", input = ".abc", expected = True }
    , { glob = "[.]abc", input = ".abc", expected = True }

    -- , { glob = "*(.)abc", input = ".abc", expected = True } -- TODO
    -- , { glob = "*(?)abc", input = ".abc", expected = True } -- TODO
    -- , { glob = "*(?|.)abc", input = ".abc", expected = True } -- TODO
    -- , { glob = "*(?|*)abc", input = ".abc", expected = True } -- TODO
    -- , { glob = "!(test)", input = ".abc", expected = True } -- TODO
    -- , { glob = "!(test)", input = "..", expected = True } -- TODO
    -- POSIX style character classes
    -- , { glob = "[[:alnum:]]bc", input = "zbc", expected = True } -- TODO
    -- , { glob = "[[:alnum:]]bc", input = "1bc", expected = True } -- TODO
    -- , { glob = "[a[:alnum:]]bc", input = "zbc", expected = True } -- TODO
    -- , { glob = "[[:alnum:][:blank:]]bc", input = " bc", expected = True } -- TODO
    -- , { glob = "*([[:word:]])", input = "WoRD5_", expected = True } -- TODO
    -- , { glob = "[[:alnum:]]bc", input = "zbc", expected = True } -- TODO
    -- , { glob = "[[:alnum:]]bc", input = "1bc", expected = True } -- TODO
    -- , { glob = "[a[:alnum:]]bc", input = "zbc", expected = True } -- TODO
    -- , { glob = "[[:alnum:][:blank:]]bc", input = " bc", expected = True } -- TODO
    -- , { glob = "*([[:word:]])", input = "WoRD5_", expected = True } -- TODO
    -- POSIX character classes are case sensitive
    -- , { glob = "[[:ALNUM:]]bc", input = "zbc", expected = False } -- TODO
    -- , { glob = "[[:AlNuM:]]bc", input = "1bc", expected = False } -- TODO
    -- We can't use a character class as a range.
    -- , { glob = "[-[:alnum:]]bc", input = "-bc", expected = True } -- TODO
    -- , { glob = "[a-[:alnum:]]bc", input = "-bc", expected = True } -- TODO
    -- , { glob = "[[:alnum:]-z]bc", input = "-bc", expected = True } -- TODO
    -- Negation
    -- , { glob = "[![:alnum:]]bc", input = "!bc", expected = True } -- TODO
    -- , { glob = "[^[:alnum:]]bc", input = "!bc", expected = True } -- TODO
    -- Negation and extended glob together
    -- `!` will be treated as an exclude pattern if it isn't followed by `(`.
    -- `(` must be escaped to exclude a name that starts with `(`.
    -- If `!(` doesn't start a valid extended glob pattern,
    -- it will be treated as a literal, not an exclude pattern.
    -- , {glob = "!\\(test)", input = "test",expected = True} -- TODO
    -- , {glob = "!(test)", input = "test",expected = False} -- TODO
    -- , {glob = "!!(test)", input = "test",expected = True} -- TODO
    -- , {glob = "!(test", input = "!(test",expected = True} -- TODO
    -- -- Backwards ranges
    , { glob = "[a-z]", input = "a", expected = True }

    -- , { glob = "[z-a]", input = "a", expected = False }
    -- , { glob = "[!z-a]", input = "a", expected = True }
    , { glob = "[!a-z]", input = "a", expected = False }

    -- , { glob = "[9--]", input = "9", expected = False }
    ]


parseTests : Test
parseTests =
    expectations
        |> Utils.parseAll
        |> describe "[wcmatch] Glob.parse"


matchTests : Test
matchTests =
    expectations
        |> Utils.checkAll
        |> describe "[wcmatch] Glob.match"
