module Main exposing (..)

import Dict exposing (Dict)
import Dict.Extra


solve str =
    str
        |> String.split ","
        |> List.filterMap String.toInt
        |> Dict.Extra.frequencies
        |> iterate 256 step
        |> Dict.values
        |> List.sum


iterate times f xs =
    case times of
        0 ->
            xs

        _ ->
            iterate (times - 1) f (f xs)


step : Dict Int Int -> Dict Int Int
step fishes =
    let
        old n =
            Dict.get n fishes |> Maybe.withDefault 0
    in
    Dict.fromList
        [ ( 0, old 1 )
        , ( 1, old 2 )
        , ( 2, old 3 )
        , ( 3, old 4 )
        , ( 4, old 5 )
        , ( 5, old 6 )
        , ( 6, old 7 + old 0 )
        , ( 7, old 8 )
        , ( 8, old 0 )
        ]
