module Main exposing (..)


solve str =
    String.lines str
        |> List.filterMap String.toInt
        |> (\list ->
                List.map3 (\a b c -> a + b + c)
                    list
                    (List.drop 1 list)
                    (List.drop 2 list)
           )
        |> List.foldl
            (\x { previous, count } ->
                case previous of
                    Nothing ->
                        { previous = Just x, count = count }

                    Just p ->
                        { previous = Just x
                        , count =
                            if x > p then
                                count + 1

                            else
                                count
                        }
            )
            { previous = Nothing, count = 0 }
        |> .count
