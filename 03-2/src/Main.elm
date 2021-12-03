module Main exposing (..)


solve : String -> Int
solve str =
    oxygen (String.lines str) * carbon (String.lines str)


oxygen : List String -> Int
oxygen lines =
    ratingHelp 0 mostCommonBit lines
        |> toBinary
        |> fromBinary


carbon : List String -> Int
carbon lines =
    ratingHelp 0 leastCommonBit lines
        |> toBinary
        |> fromBinary


ratingHelp : Int -> (Int -> List String -> Bit) -> List String -> String
ratingHelp index criteria lines =
    case lines of
        [] ->
            ""

        [ report ] ->
            report

        reports ->
            let
                critBit =
                    criteria index lines
            in
            reports
                |> List.filter (\report -> nChar index report == "1" && critBit == One || nChar index report == "0" && critBit == Zero)
                |> ratingHelp (index + 1) criteria


nChar : Int -> String -> String
nChar index str =
    String.slice index (index + 1) str


mostCommonBit : Int -> List String -> Bit
mostCommonBit index lines =
    let
        ( ones, zeroes ) =
            lines
                |> List.foldl
                    (\report ( ones_, zeroes_ ) ->
                        case nChar index report of
                            "1" ->
                                ( ones_ + 1, zeroes_ )

                            _ ->
                                ( ones_, zeroes_ + 1 )
                    )
                    ( 0, 0 )
    in
    if ones >= zeroes then
        One

    else
        Zero


leastCommonBit : Int -> List String -> Bit
leastCommonBit index lines =
    mostCommonBit index lines |> flip


flip : Bit -> Bit
flip bit =
    if bit == One then
        Zero

    else
        One


type Bit
    = One
    | Zero


toBinary : String -> List Bit
toBinary str =
    str
        |> String.toList
        |> List.map
            (\char ->
                case char of
                    '1' ->
                        One

                    _ ->
                        Zero
            )


fromBinary : List Bit -> Int
fromBinary bits =
    bits
        |> List.foldr
            (\bit ( index, sum ) ->
                case bit of
                    One ->
                        ( index + 1, sum + 2 ^ index )

                    Zero ->
                        ( index + 1, sum )
            )
            ( 0, 0 )
        |> (\( _, sum ) -> sum)
