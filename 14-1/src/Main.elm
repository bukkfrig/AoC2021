module Main exposing (..)

import Dict exposing (Dict)
import Dict.Extra


solve str =
    let
        ( template, rules ) =
            parse str

        frequencies =
            template
                |> iterate 10 (construct rules)
                |> Dict.Extra.frequencies
                |> Dict.values
    in
    Maybe.map2 (-) (List.maximum frequencies) (List.minimum frequencies)
        |> orElseCrash


parse : String -> ( List Char, Dict ( Char, Char ) Char )
parse str =
    case String.lines str of
        template :: _ :: rules ->
            ( String.toList template, List.map parseRule rules |> Dict.fromList )

        _ ->
            Debug.todo "Invalid input"


parseRule : String -> ( ( Char, Char ), Char )
parseRule str =
    case String.split " -> " str of
        pair :: insert :: [] ->
            case ( String.toList pair, String.toList insert ) of
                ( a :: b :: [], c :: [] ) ->
                    ( ( a, b ), c )

                _ ->
                    Debug.todo "Invalid input"

        _ ->
            Debug.todo "Invalid input"


construct : Dict ( Char, Char ) Char -> List Char -> List Char
construct rules template =
    let
        pairs =
            List.map2 Tuple.pair
                template
                (template |> List.drop 1)
    in
    (List.map2 (\first pair -> List.filterMap identity [ Just first, Dict.get pair rules ])
        template
        pairs
        |> List.concatMap identity
    )
        ++ [ List.head (List.reverse template) |> orElseCrash ]


iterate : Int -> (a -> a) -> a -> a
iterate times f xs =
    case times of
        0 ->
            xs

        _ ->
            iterate (times - 1) f (f xs)


orElseCrash : Maybe a -> a
orElseCrash maybe =
    case maybe of
        Just a ->
            a

        Nothing ->
            Debug.todo ""
