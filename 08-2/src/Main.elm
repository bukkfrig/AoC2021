module Main exposing (..)

import Dict exposing (Dict)
import List.Extra


solve str =
    str
        |> String.lines
        |> List.map parseLine
        |> List.map (Tuple.mapFirst decipher)
        |> List.map (\( legend, readings ) -> List.map (canonicalize >> lookup legend) readings)
        |> List.map fromDigits
        |> List.sum


parseLine : String -> ( List String, List String )
parseLine str =
    case String.words str of
        one :: two :: three :: four :: five :: six :: seven :: eight :: nine :: ten :: separator :: a :: b :: c :: d :: [] ->
            ( [ one, two, three, four, five, six, seven, eight, nine, ten ], [ a, b, c, d ] )

        _ ->
            Debug.todo "Invalid input"


lookup : Dict comparable b -> comparable -> b
lookup dict comparable =
    Dict.get comparable dict |> orElseCrash


fromDigits : List Int -> Int
fromDigits =
    List.foldl (\n acc -> n + acc * 10) 0


canonicalize : String -> String
canonicalize =
    String.toList >> List.sort >> String.fromList


decipher : List String -> Dict String Int
decipher readings =
    let
        one =
            readings |> find (\it -> String.length it == 2)

        four =
            readings |> find (\it -> String.length it == 4)

        seven =
            readings |> find (\it -> String.length it == 3)

        eight =
            readings |> find (\it -> String.length it == 7)

        six =
            readings |> find (\it -> String.length it == 6 && not (it |> contains one))

        nine =
            readings |> find (\it -> String.length it == 6 && (it |> contains four))

        zero =
            readings |> find (\it -> String.length it == 6 && not (it == six || it == nine))

        three =
            readings |> find (\it -> String.length it == 5 && (it |> contains one))

        five =
            readings |> find (\it -> String.length it == 5 && (six |> contains it))

        two =
            readings |> find (\it -> String.length it == 5 && not (it == three || it == five))
    in
    List.Extra.zip [ zero, one, two, three, four, five, six, seven, eight, nine ] (List.range 0 9)
        |> List.map (Tuple.mapFirst canonicalize)
        |> Dict.fromList


find : (x -> Bool) -> List x -> x
find f xs =
    List.Extra.find f xs |> orElseCrash


orElseCrash : Maybe a -> a
orElseCrash maybe =
    case maybe of
        Just a ->
            a

        Nothing ->
            Debug.todo ""


contains : String -> String -> Bool
contains reading1 reading2 =
    reading1
        |> String.toList
        |> List.all (\c -> List.member c (String.toList reading2))
