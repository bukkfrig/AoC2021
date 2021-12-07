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
        |> List.map (\pos -> ( pos, List.map (\x -> fuel x pos) start ))
        |> List.map Tuple.second
        |> List.map List.sum
        |> List.minimum


fuel x pos =
    let
        fuelhelp target ( total, fuelcost, cost ) =
            if total + 1 >= target then
                fuelcost + cost

            else
                fuelhelp target ( total + 1, fuelcost + cost, cost + 1 )
    in
    if x == pos then
        0

    else
        fuelhelp (abs (x - pos)) ( 0, 0, 1 )
