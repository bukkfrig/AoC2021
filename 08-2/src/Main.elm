module Main exposing (..)

import Dict exposing (Dict)
import List.Extra


solve str =
    str
        |> String.lines
        |> List.map parseLine
        |> List.map compute
        |> List.sum


parseLine : String -> ( List String, List String )
parseLine str =
    case String.words str of
        one :: two :: three :: four :: five :: six :: seven :: eight :: nine :: ten :: separator :: a :: b :: c :: d :: [] ->
            ( [ one, two, three, four, five, six, seven, eight, nine, ten ], [ a, b, c, d ] )

        _ ->
            Debug.todo "Invalid input"


compute : ( List String, List String ) -> Int
compute ( readings, digitStrings ) =
    digitStrings
        |> List.map (String.toList >> List.sort >> String.fromList)
        |> List.map (\str -> Dict.get str (solvereadings readings) |> Maybe.withDefault -99999)
        |> fromDigits


fromDigits : List Int -> Int
fromDigits =
    List.foldr (\n ( place, total ) -> ( place + 1, total + n * (10 ^ place) )) ( 0, 0 ) >> Tuple.second


sort : String -> String
sort =
    String.toList >> List.sort >> String.fromList


solvereadings : List String -> Dict String Int
solvereadings readings =
    let
        one =
            readings |> List.Extra.find (\reading -> String.length reading == 2) |> Maybe.withDefault ""

        four =
            readings |> List.Extra.find (\reading -> String.length reading == 4) |> Maybe.withDefault ""

        seven =
            readings |> List.Extra.find (\reading -> String.length reading == 3) |> Maybe.withDefault ""

        eight =
            readings |> List.Extra.find (\reading -> String.length reading == 7) |> Maybe.withDefault ""

        six =
            readings |> List.Extra.find (\reading -> String.length reading == 6 && not (reading |> contains one)) |> Maybe.withDefault ""

        nine =
            readings |> List.Extra.find (\reading -> String.length reading == 6 && (reading |> contains four)) |> Maybe.withDefault ""

        zero =
            readings |> List.Extra.find (\reading -> String.length reading == 6 && not (reading == six || reading == nine)) |> Maybe.withDefault ""

        three =
            readings |> List.Extra.find (\reading -> String.length reading == 5 && (reading |> contains one)) |> Maybe.withDefault ""

        five =
            readings |> List.Extra.find (\reading -> String.length reading == 5 && (six |> contains reading)) |> Maybe.withDefault ""

        two =
            readings |> List.Extra.find (\reading -> String.length reading == 5 && not (six |> contains reading) && not (reading == three)) |> Maybe.withDefault ""
    in
    List.map2 Tuple.pair
        ([ zero, one, two, three, four, five, six, seven, eight, nine ] |> List.map sort)
        (List.range 0 9)
        |> Dict.fromList


contains reading1 reading2 =
    reading1
        |> String.toList
        |> List.any (\c -> List.member c (reading2 |> String.toList) |> not)
        |> not
