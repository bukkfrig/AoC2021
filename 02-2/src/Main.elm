module Main exposing (main)

import Html


main : Html.Html msg
main = 
    String.lines input
        |> List.filterMap instructionFromString
        |> List.foldl move { horizontal = 0, depth = 0 }
        |> (\{ horizontal, depth } -> horizontal * depth)
        |> String.fromInt
        |> Html.text


type alias Position = { horizontal: Int, depth: Int }

type Instruction
    = Forward Int
    | Down Int
    | Up Int

move : Instruction -> Position -> Position
move instruction position =
    case instruction of
        Forward n -> { position | horizontal = position.horizontal + n }
        Down n -> { position | depth = position.depth + n }
        Up n -> { position | depth = position.depth - n }

instructionFromString: String -> Maybe Instruction
instructionFromString str =
    case String.split " " str of
        dirStr :: nStr :: [] ->
            case (dirStr, String.toInt nStr) of
                ("forward", Just n) -> Just (Forward n)
                ("down", Just n) -> Just (Down n)
                ("up", Just n) -> Just (Up n)
                _ -> Nothing
        _ -> Nothing




input = Debug.todo ""