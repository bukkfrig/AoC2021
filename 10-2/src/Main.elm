module Main exposing (..)

import Dict exposing (Dict)
import Dict.Extra


solve str =
    str
        |> String.lines
        |> List.map String.toList
        |> List.map parseLine
        |> List.filterMap
            (\result ->
                case result of
                    Err (Incomplete stack) ->
                        Just (autoComplete stack)

                    _ ->
                        Nothing
            )
        |> List.map scoreAutocomplete
        |> (\list -> list |> List.sort |> List.drop (List.length list // 2) |> List.head |> orElseCrash)


type alias Stack a =
    List a


type alias Opener =
    Char


type alias Closer =
    Char


type ChunkError
    = Corrupted Opener
    | Incomplete (Stack Opener)


parseLine : List Char -> Result ChunkError ()
parseLine line =
    parseLineHelp line []


parseLineHelp : List Char -> Stack Opener -> Result ChunkError ()
parseLineHelp remaining stack =
    case remaining of
        [] ->
            if List.isEmpty stack then
                Ok ()

            else
                Err (Incomplete stack)

        x :: xs ->
            if List.member x openers then
                parseLineHelp xs (x :: stack)

            else
                case stack of
                    [] ->
                        Err (Corrupted x)

                    opener :: rest ->
                        if open x /= opener then
                            Err (Corrupted x)

                        else
                            parseLineHelp xs rest


pairs : Dict Opener Closer
pairs =
    Dict.fromList
        [ ( '(', ')' )
        , ( '[', ']' )
        , ( '{', '}' )
        , ( '<', '>' )
        ]


openers : List Opener
openers =
    Dict.keys pairs


orElseCrash : Maybe a -> a
orElseCrash maybe =
    case maybe of
        Just a ->
            a

        Nothing ->
            Debug.todo ""


close : Opener -> Closer
close c =
    Dict.get c pairs |> orElseCrash


invertedPairs : Dict Closer Opener
invertedPairs =
    Dict.Extra.invert pairs


open : Closer -> Opener
open c =
    Dict.get c invertedPairs |> orElseCrash


autoComplete : Stack Opener -> List Closer
autoComplete stack =
    autoCompleteHelp stack [] |> List.reverse


autoCompleteHelp : Stack Opener -> List Closer -> List Closer
autoCompleteHelp stack output =
    case stack of
        [] ->
            output

        x :: xs ->
            autoCompleteHelp xs (close x :: output)


autoCompletePoints : Dict Closer Int
autoCompletePoints =
    Dict.fromList
        [ ( ')', 1 )
        , ( ']', 2 )
        , ( '}', 3 )
        , ( '>', 4 )
        ]


scoreAutocomplete : List Closer -> Int
scoreAutocomplete =
    List.foldl (\c acc -> acc * 5 + (Dict.get c autoCompletePoints |> orElseCrash)) 0
