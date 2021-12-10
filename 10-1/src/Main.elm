module Main exposing (..)


solve str =
    let
        chunks =
            str
                |> String.lines
                |> List.map parseLine
    in
    chunks
        |> List.map parseChunk
        |> List.filterMap
            (\result ->
                case result of
                    Err (Corrupted c) ->
                        Just (errorValue c)

                    _ ->
                        Nothing
            )
        |> List.sum


type ChunkError
    = Corrupted Char
    | Incomplete


parseLine : String -> List Char
parseLine =
    String.toList


parseChunk : List Char -> Result ChunkError ()
parseChunk chunk =
    parseChunkHelp chunk []


parseChunkHelp : List Char -> List Char -> Result ChunkError ()
parseChunkHelp chunk stack =
    case chunk of
        c :: cs ->
            case c of
                '(' ->
                    parseChunkHelp cs (c :: stack)

                '[' ->
                    parseChunkHelp cs (c :: stack)

                '{' ->
                    parseChunkHelp cs (c :: stack)

                '<' ->
                    parseChunkHelp cs (c :: stack)

                ')' ->
                    '(' |> expectPoppedThen (parseChunkHelp cs) stack c

                ']' ->
                    '[' |> expectPoppedThen (parseChunkHelp cs) stack c

                '}' ->
                    '{' |> expectPoppedThen (parseChunkHelp cs) stack c

                '>' ->
                    '<' |> expectPoppedThen (parseChunkHelp cs) stack c

                _ ->
                    Debug.todo "Invalid character"

        [] ->
            Ok ()


expectPoppedThen : (List Char -> Result ChunkError ()) -> List Char -> Char -> Char -> Result ChunkError ()
expectPoppedThen return stack actual expect =
    case stack of
        popped :: rest ->
            if popped == expect then
                return rest

            else
                Err (Corrupted actual)

        [] ->
            Err Incomplete


errorValue : Char -> Int
errorValue c =
    case c of
        ')' ->
            3

        ']' ->
            57

        '}' ->
            1197

        '>' ->
            25137

        _ ->
            Debug.todo ""
