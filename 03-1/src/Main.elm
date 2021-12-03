module Main exposing (..)


solve : String -> Int
solve str =
    gamma (String.lines str) * epsilon (String.lines str)


gamma : List String -> Int
gamma lines =
    let
        bits =
            List.head lines |> Maybe.map String.length |> Maybe.withDefault 0
    in
    List.range 0 (bits - 1)
        |> List.map
            (\index -> 
                List.foldl
                    (\report (ones, zeroes) ->
                        case String.slice index (index + 1) report of
                           "1" -> (ones + 1, zeroes)
                           "0" -> (ones, zeroes + 1)
                           _ -> (ones, zeroes)
                    )
                    (0,0)
                    lines
            )
            |> List.map (\(ones, zeroes) -> if ones > zeroes then One else Zero)
            |> fromBinary

type Bit = One | Zero

fromBinary : List Bit -> Int
fromBinary bits =
    List.foldr
        (\bit ( index, sum ) ->
            case bit of
                One ->
                    ( index + 1, sum + 2 ^ index )

                Zero ->
                    ( index + 1, sum )
        )
        ( 0, 0 )
        bits
        |> (\( _, sum ) -> sum)


epsilon : List String -> Int
epsilon lines =
    let
        bits =
            List.head lines |> Maybe.map String.length |> Maybe.withDefault 0
    in
    List.range 0 (bits - 1)
        |> List.map
            (\index -> 
                List.foldl
                    (\report (ones, zeroes) ->
                        case String.slice index (index + 1) report of
                           "1" -> (ones + 1, zeroes)
                           "0" -> (ones, zeroes + 1)
                           _ -> (ones, zeroes)
                    )
                    (0,0)
                    lines
            )
            |> List.map (\(ones, zeroes) -> if ones < zeroes then One else Zero)
            |> fromBinary
