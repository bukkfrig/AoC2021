module Main exposing (..)

import Dict exposing (Dict)
import Dict.Extra


solve str =
    str
        |> String.split ","
        |> List.filterMap String.toInt
        |> Dict.Extra.groupBy identity
        |> Dict.map (\_ fishes -> List.length fishes)
        |> iterate 256
        |> Dict.values
        |> List.sum


iterate times fishes =
    case times of
        0 ->
            fishes

        _ ->
            iterate (times - 1) (step fishes)


step : Dict Int Int -> Dict Int Int
step fishes =
    fishes
        |> Dict.update 0 (always (Dict.get 1 fishes |> Maybe.withDefault 0 |> Just))
        |> Dict.update 1 (always (Dict.get 2 fishes |> Maybe.withDefault 0 |> Just))
        |> Dict.update 2 (always (Dict.get 3 fishes |> Maybe.withDefault 0 |> Just))
        |> Dict.update 3 (always (Dict.get 4 fishes |> Maybe.withDefault 0 |> Just))
        |> Dict.update 4 (always (Dict.get 5 fishes |> Maybe.withDefault 0 |> Just))
        |> Dict.update 5 (always (Dict.get 6 fishes |> Maybe.withDefault 0 |> Just))
        |> Dict.update 6 (always (((Dict.get 7 fishes |> Maybe.withDefault 0) + (Dict.get 0 fishes |> Maybe.withDefault 0)) |> Just))
        |> Dict.update 7 (always (Dict.get 8 fishes |> Maybe.withDefault 0 |> Just))
        |> Dict.update 8 (always (Dict.get 0 fishes |> Maybe.withDefault 0 |> Just))
