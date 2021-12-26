module Main exposing (..)

import Dict exposing (Dict)
import Dict.Extra


solve str =
    let
        ( template, rules ) =
            parse str

        frequencies =
            ( pairCountFromTemplate template, Dict.Extra.frequencies template )
                |> iterate 40 (updateFrequencies rules)
                |> Tuple.second
                |> Dict.values
    in
    Maybe.map2 (-) (List.maximum frequencies) (List.minimum frequencies)
        |> orElseCrash


parse : String -> ( List Char, Ruleset )
parse str =
    case String.lines str of
        template :: _ :: rules ->
            ( String.toList template
            , Dict.fromList (List.map parseRule rules)
            )

        _ ->
            Debug.todo "Invalid input"


parseRule : String -> ( ( Char, Char ), Char )
parseRule str =
    case String.split " -> " str of
        [ pair, insert ] ->
            case ( String.toList pair, String.toList insert ) of
                ( [ a, b ], [ c ] ) ->
                    ( ( a, b ), c )

                _ ->
                    Debug.todo "Invalid input"

        _ ->
            Debug.todo "Invalid input"


pairCountFromTemplate : List Char -> PairCounts
pairCountFromTemplate template =
    List.map2 Tuple.pair
        template
        (template |> List.drop 1)
        |> Dict.Extra.frequencies


type alias Ruleset =
    Dict ( Char, Char ) Char


type alias PairCounts =
    Dict ( Char, Char ) Int


type alias LetterCounts =
    Dict Char Int


updateFrequencies : Ruleset -> (( PairCounts, LetterCounts ) -> ( PairCounts, LetterCounts ))
updateFrequencies rules ( pairCounts, letterCounts ) =
    Dict.foldl
        (\( a, b ) count ( pairCounts_, letterCounts_ ) ->
            let
                inserted =
                    Dict.get ( a, b ) rules |> orElseCrash
            in
            ( pairCounts_
                |> Dict.update ( a, b ) (Maybe.map (\latest -> latest - count) >> orElseCrash >> Just)
                |> Dict.update ( a, inserted ) (Maybe.withDefault 0 >> (+) count >> Just)
                |> Dict.update ( inserted, b ) (Maybe.withDefault 0 >> (+) count >> Just)
            , letterCounts_
                |> Dict.update inserted (Maybe.withDefault 0 >> (+) count >> Just)
            )
        )
        ( pairCounts, letterCounts )
        pairCounts


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
