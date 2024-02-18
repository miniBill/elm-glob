module Glob exposing (Glob, parse, match)

{-|

@docs Glob, parse, match

-}

import Json.Encode
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
    | Class { negative : Bool, inner : String }
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
            if matchComponents components stail then
                True

            else
                matchComponents ctail segments

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
        , Parser.succeed (\before parsed after source -> ( parsed, String.slice before after source ))
            |= Parser.getOffset
            |= Parser.sequence
                { start = ""
                , end = ""
                , separator = ""
                , trailing = Parser.Optional
                , spaces = Parser.succeed ()
                , item = fragmentParser
                }
            |= Parser.getOffset
            |= Parser.getSource
            |> Parser.andThen
                (\( fragments, original ) ->
                    Parser.succeed (\regex -> Fragments ( fragments, regex ))
                        |= fragmentsToRegex original fragments
                )
        ]


fragmentsToRegex : String -> List Fragment -> Parser Regex
fragmentsToRegex original fragments =
    let
        regexString : String
        regexString =
            fragments
                |> List.map fragmentToRegexString
                |> String.concat
    in
    case Regex.fromStringWith { caseInsensitive = False, multiline = True } ("^" ++ regexString ++ "$") of
        Nothing ->
            Parser.problem <|
                "Could not parse "
                    ++ escape regexString
                    ++ " as a regex, obtained from "
                    ++ original

        Just regex ->
            Parser.succeed regex


escape : String -> String
escape input =
    input
        |> Json.Encode.string
        |> Json.Encode.encode 0


fragmentToRegexString : Fragment -> String
fragmentToRegexString fragment =
    case fragment of
        Literal literal ->
            regexEscape literal

        Alternatives alternatives ->
            "(" ++ String.join "|" (List.map regexEscape <| Set.toList alternatives) ++ ")"

        Class { negative, inner } ->
            let
                cut : String
                cut =
                    inner
                        |> String.replace "^" "\\^"
                        |> String.replace "\\" "\\\\"
            in
            if negative then
                "[^" ++ cut ++ "]"

            else
                "[" ++ cut ++ "]"

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
                            [ '[', c, ']' ]
            )
        |> String.fromList


fragmentParser : Parser Fragment
fragmentParser =
    Parser.oneOf
        [ Parser.succeed Literal
            |. Parser.symbol "\\"
            |= Parser.getChompedString (Parser.chompIf (\_ -> True))
        , Parser.succeed QuestionMark
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
                , item = nonemptyChomper <| \c -> notSpecial c && c /= ','
                }
        , Parser.succeed (\negative inner -> Class { negative = negative, inner = inner })
            |. Parser.symbol "["
            |= Parser.oneOf
                [ Parser.succeed True
                    |. Parser.symbol "!"
                , Parser.succeed False
                ]
            |= Parser.getChompedString
                (Parser.succeed ()
                    |. Parser.oneOf [ Parser.symbol "]", Parser.succeed () ]
                    |. Parser.chompWhile (\c -> c /= ']')
                )
            |. Parser.symbol "]"
        , Parser.succeed Literal
            |= nonemptyChomper notSpecial
        , Parser.problem "fragmentParser"
        ]


nonemptyChomper : (Char -> Bool) -> Parser String
nonemptyChomper f =
    Parser.getChompedString
        (Parser.chompIf f
            |. Parser.chompWhile f
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
