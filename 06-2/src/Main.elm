module Main exposing (..)

import Dict exposing (Dict)
import Dict.Extra


solve str =
    str
        |> String.split ","
        |> List.filterMap String.toInt
        |> Dict.Extra.groupBy identity
        |> Dict.map (\_ fishes -> List.length fishes)
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
    fishes
        |> Dict.insert 0 (old 1)
        |> Dict.insert 1 (old 2)
        |> Dict.insert 2 (old 3)
        |> Dict.insert 3 (old 4)
        |> Dict.insert 4 (old 5)
        |> Dict.insert 5 (old 6)
        |> Dict.insert 6 (old 7 + old 0)
        |> Dict.insert 7 (old 8)
        |> Dict.insert 8 (old 0)
