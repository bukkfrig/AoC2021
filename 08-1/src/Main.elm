module Main exposing (..)


solve str =
    str
        |> String.lines
        |> List.map parseLine
        |> List.map Tuple.second
        |> List.concatMap (List.map String.length)
        |> List.filter (\x -> List.member x uniqueDigitCount)
        |> List.length


parseLine : String -> ( List String, List String )
parseLine str =
    case String.words str of
        one :: two :: three :: four :: five :: six :: seven :: eight :: nine :: ten :: separator :: a :: b :: c :: d :: [] ->
            ( [ one, two, three, four, five, six, seven, eight, nine, ten ], [ a, b, c, d ] )

        _ ->
            Debug.todo "Invalid input"


uniqueDigitCount =
    [ 2, 4, 3, 7 ]
