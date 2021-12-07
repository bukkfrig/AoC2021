module Main exposing (..)


solve str =
    let
        start =
            str
                |> String.split ","
                |> List.filterMap String.toInt

        max =
            List.maximum start |> Maybe.withDefault 0
    in
    List.range 0 max
        |> List.map (\pos -> ( pos, List.map (\x -> abs (x - pos)) start ))
        |> List.map Tuple.second
        |> List.map List.sum
        |> List.minimum
