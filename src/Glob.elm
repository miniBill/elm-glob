module Glob exposing (Glob, parse, match)

{-|

@docs Glob, parse, match

-}

import Parser exposing (Parser)
import Regex exposing (Regex)
import Set exposing (Set)


{-| -}
type Glob
    = Glob Regex


{-| -}
match : String -> String -> Bool
match glob input =
    case parse glob of
        Err _ ->
            False

        Ok (Glob parsed) ->
            Regex.contains parsed input


type Fragment
    = Literal String
    | Slash
    | Alternatives (Set String)
    | QuestionMark
    | Asterisk
    | TwoAsterisks


{-| -}
parse : String -> Result (List Parser.DeadEnd) Glob
parse input =
    Parser.run parser input
        |> Result.map fragmentsToGlob


fragmentsToGlob : List Fragment -> Glob
fragmentsToGlob fragments =
    fragments
        |> List.map fragmentToRegexString
        |> String.concat
        |> (\s -> "^" ++ s ++ "$")
        |> Regex.fromString
        |> Maybe.withDefault Regex.never
        |> Glob


fragmentToRegexString : Fragment -> String
fragmentToRegexString fragment =
    Debug.todo "TODO"


parser : Parser (List Fragment)
parser =
    Parser.sequence
        { start = ""
        , end = ""
        , separator = ""
        , trailing = Parser.Optional
        , spaces = Parser.succeed ()
        , item = fragmentParser
        }


fragmentParser : Parser Fragment
fragmentParser =
    Parser.problem "TODO"
