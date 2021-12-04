module Main exposing (..)

import List.Extra


solve : String -> Int
solve str =
    case String.lines str of
        first :: rest ->
            let
                calledNumbers =
                    first
                        |> String.split ","
                        |> List.filterMap String.toInt

                boards =
                    parseBoards rest []
            in
            play calledNumbers boards

        [] ->
            Debug.todo "No input"


parseBoard : List String -> Board
parseBoard lines =
    Board
        (lines
            |> List.map (String.words >> List.filterMap String.toInt >> List.map (\value -> { value = value, called = False }))
        )


parseBoards : List String -> List Board -> List Board
parseBoards list acc =
    case list of
        _ :: b :: c :: d :: e :: f :: remain ->
            parseBoard [ b, c, d, e, f ] :: parseBoards remain acc

        _ ->
            acc


play : List Int -> List Board -> Int
play nums state =
    case nums of
        x :: xs ->
            let
                newstate =
                    List.map (callBoard x) state
            in
            case List.head (List.filter isWinner newstate) of
                Just winner ->
                    score x winner

                Nothing ->
                    play xs newstate

        _ ->
            Debug.todo "No winners"


type alias Cell =
    { value : Int, called : Bool }


type Board
    = Board (List (List Cell))


call : Int -> Cell -> Cell
call num cell =
    if cell.value == num then
        { cell | called = True }

    else
        cell


callRow : Int -> List Cell -> List Cell
callRow n =
    List.map (call n)


callBoard : Int -> Board -> Board
callBoard n (Board rows) =
    Board (List.map (callRow n) rows)


isWinner : Board -> Bool
isWinner (Board rows) =
    let
        wonWithRow =
            rows
                |> List.any (\row -> List.all .called row)

        wonWithColumn =
            rows
                |> List.Extra.transpose
                |> List.any (\row -> List.all .called row)
    in
    wonWithRow || wonWithColumn


score : Int -> Board -> Int
score n (Board rows) =
    n
        * (rows
            |> List.map scoreRow
            |> List.sum
          )


scoreRow : List Cell -> Int
scoreRow row =
    row
        |> List.filter (.called >> not)
        |> List.map .value
        |> List.sum
