module Glob exposing (Glob, parse, match)

{-|

@docs Glob, parse, match

-}

import Parser exposing ((|.), (|=), Parser)
import Regex exposing (Regex)
import Set exposing (Set)


{-| -}
type Glob
    = Glob (List Component)


type Component
    = TwoAsterisks
    | Fragments ( List Fragment, Regex )


type Fragment
    = Literal String
    | Alternatives (Set String)
    | QuestionMark
    | Asterisk


{-| -}
match : String -> String -> Bool
match glob input =
    case parse glob of
        Err _ ->
            False

        Ok (Glob parsed) ->
            matchComponents parsed (String.split "/" input)


matchComponents : List Component -> List String -> Bool
matchComponents components segments =
    case ( components, segments ) of
        ( [], [] ) ->
            True

        ( _ :: _, [] ) ->
            False

        ( [], _ :: _ ) ->
            False

        ( TwoAsterisks :: ctail, _ :: stail ) ->
            matchComponents components stail || matchComponents ctail segments

        ( (Fragments ( _, chead )) :: ctail, shead :: stail ) ->
            if Regex.contains chead shead then
                matchComponents ctail stail

            else
                False


{-| -}
parse : String -> Result (List Parser.DeadEnd) Glob
parse input =
    input
        |> Parser.run parser
        |> Result.map Glob
        |> Debug.log ("Parsed " ++ input ++ " as ")


parser : Parser (List Component)
parser =
    Parser.sequence
        { start = ""
        , end = ""
        , separator = "/"
        , trailing = Parser.Optional
        , spaces = Parser.succeed ()
        , item = componentParser
        }
        |. Parser.end


componentParser : Parser Component
componentParser =
    Parser.oneOf
        [ Parser.succeed TwoAsterisks
            |. Parser.symbol "**"
        , Parser.succeed (\fragments -> Fragments ( fragments, fragmentsToRegex fragments ))
            |= Parser.sequence
                { start = ""
                , end = ""
                , separator = ""
                , trailing = Parser.Optional
                , spaces = Parser.succeed ()
                , item = fragmentParser
                }
        ]


fragmentsToRegex : List Fragment -> Regex
fragmentsToRegex fragments =
    let
        regexString : String
        regexString =
            fragments
                |> List.map fragmentToRegexString
                |> String.concat
    in
    case Regex.fromString ("^" ++ regexString ++ "$") of
        Nothing ->
            let
                _ =
                    Debug.todo <|
                        "Could not parse "
                            ++ regexString
                            ++ " obtained from "
                            ++ Debug.toString regexString
            in
            Regex.never

        Just regex ->
            regex


fragmentToRegexString : Fragment -> String
fragmentToRegexString fragment =
    case fragment of
        Literal literal ->
            regexEscape literal

        Alternatives alternatives ->
            "(" ++ String.join "|" (List.map regexEscape <| Set.toList alternatives) ++ ")"

        QuestionMark ->
            "."

        Asterisk ->
            ".*"


regexEscape : String -> String
regexEscape input =
    input
        |> String.toList
        |> List.concatMap
            (\c ->
                if Char.isAlphaNum c then
                    [ c ]

                else
                    case c of
                        '\\' ->
                            [ '\\', '\\' ]

                        ']' ->
                            [ '\\', c ]

                        _ ->
                            -- [ '[', c, ']' ]
                            [ '\\', c ]
            )
        |> String.fromList


fragmentParser : Parser Fragment
fragmentParser =
    Parser.oneOf
        [ Parser.succeed QuestionMark
            |. Parser.symbol "?"
        , Parser.succeed Asterisk
            |. Parser.symbol "*"
        , Parser.succeed (Alternatives << Set.fromList)
            |= Parser.sequence
                { start = "{"
                , end = "}"
                , separator = ","
                , trailing = Parser.Forbidden
                , spaces = Parser.succeed ()
                , item = literalParser
                }
        , Parser.succeed Literal
            |= literalParser
        , Parser.problem "fragmentParser"
        ]


literalParser : Parser String
literalParser =
    Parser.getChompedString
        (Parser.chompIf notSpecial
            |. Parser.chompWhile notSpecial
        )


specialChars : Set Char
specialChars =
    [ '*'
    , '{'
    , '}'
    , '['
    , ']'
    , '?'
    , '/'
    ]
        |> Set.fromList


notSpecial : Char -> Bool
notSpecial c =
    not (Set.member c specialChars)
