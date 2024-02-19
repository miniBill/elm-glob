module Utils exposing (Expectation, checkAll, parseAll)

import Expect
import Glob
import Json.Encode
import Parser
import Set
import Test exposing (Test, test)


type alias Expectation =
    { glob : String
    , input : String
    , expected : Bool
    }


parseAll : List Expectation -> List Test
parseAll expectations =
    expectations
        |> List.map .glob
        |> Set.fromList
        |> Set.toList
        |> List.map
            (\glob ->
                test ("Glob: " ++ escape glob) <|
                    \_ ->
                        case Glob.fromString glob of
                            Err [ single ] ->
                                case single.problem of
                                    Parser.Problem message ->
                                        Expect.fail <| "Failed to parse: " ++ message

                                    _ ->
                                        Expect.fail <| "Failed to parse:[] " ++ Debug.toString e

                            Err e ->
                                Expect.fail <| "Failed to parse: " ++ Debug.toString e

                            Ok _ ->
                                Expect.pass
            )


checkAll : List Expectation -> List Test
checkAll expectations =
    expectations
        |> List.map
            (\{ glob, input, expected } ->
                test ("Glob: " ++ escape glob ++ " Input: " ++ escape input)
                    (\() ->
                        case Glob.fromString glob of
                            Err [ single ] ->
                                case single.problem of
                                    Parser.Problem message ->
                                        Expect.fail <| "Failed to parse: " ++ message

                                    _ ->
                                        Expect.fail <| "Failed to parse:[] " ++ Debug.toString e

                            Err e ->
                                Expect.fail <| "Failed to parse: " ++ Debug.toString e

                            Ok validGlob ->
                                Glob.match validGlob input
                                    |> Expect.equal expected
                                    |> Expect.onFail "Should not have matched"
                    )
            )


escape : String -> String
escape input =
    input
        |> Json.Encode.string
        |> Json.Encode.encode 0
        |> String.slice 1 -1
