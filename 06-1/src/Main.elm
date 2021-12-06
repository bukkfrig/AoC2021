module Main exposing (..)


solve str =
    str
        |> String.split ","
        |> List.filterMap String.toInt
        |> iterate 80
        |> List.length


iterate times fishes =
    case times of
        0 ->
            fishes

        _ ->
            iterate (times - 1) (step fishes)


step : List Int -> List Int
step =
    List.concatMap
        (\age ->
            if age == 0 then
                [ 8, 6 ]

            else
                [ age - 1 ]
        )
