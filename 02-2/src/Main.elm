module Main exposing (..)


solve str =
    String.lines str
        |> List.filterMap instructionFromString
        |> List.foldl move { horizontal = 0, depth = 0, aim = 0 }
        |> (\{ horizontal, depth } -> horizontal * depth)


type alias Position =
    { horizontal : Int, depth : Int, aim : Int }


type Instruction
    = Forward Int
    | Down Int
    | Up Int


move : Instruction -> Position -> Position
move instruction position =
    case instruction of
        Forward n ->
            { position | horizontal = position.horizontal + n, depth = position.depth + position.aim * n }

        Down n ->
            { position | aim = position.aim + n }

        Up n ->
            { position | aim = position.aim - n }


instructionFromString : String -> Maybe Instruction
instructionFromString str =
    case String.split " " str of
        dirStr :: nStr :: [] ->
            case ( dirStr, String.toInt nStr ) of
                ( "forward", Just n ) ->
                    Just (Forward n)

                ( "down", Just n ) ->
                    Just (Down n)

                ( "up", Just n ) ->
                    Just (Up n)

                _ ->
                    Nothing

        _ ->
            Nothing
