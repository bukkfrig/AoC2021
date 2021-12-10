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
                    Err (Incomplete closing) ->
                        Just (scoreAutocomplete closing)

                    _ ->
                        Nothing
            )
        |> List.sort
        |> (\list -> List.drop (List.length list // 2) list |> List.head)


scoreAutocomplete : List Char -> Int
scoreAutocomplete =
    List.foldl (\c acc -> acc * 5 + autoCompletePoints c) 0


type ChunkError
    = Corrupted Char
    | Incomplete (List Char)


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
            if stack == [] then
                Ok ()

            else
                Err (Incomplete (autoComplete stack))


autoComplete : List Char -> List Char
autoComplete stack =
    autoCompleteHelp stack []
        |> List.reverse


autoCompleteHelp : List Char -> List Char -> List Char
autoCompleteHelp stack output =
    case stack of
        [] ->
            output

        x :: xs ->
            autoCompleteHelp xs (close x :: output)


expectPoppedThen : (List Char -> Result ChunkError ()) -> List Char -> Char -> Char -> Result ChunkError ()
expectPoppedThen return stack actual expect =
    case stack of
        popped :: rest ->
            if popped == expect then
                return rest

            else
                Err (Corrupted actual)

        [] ->
            Err (Corrupted actual)


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


close : Char -> Char
close c =
    case c of
        '(' ->
            ')'

        '[' ->
            ']'

        '{' ->
            '}'

        '<' ->
            '>'

        _ ->
            Debug.todo ""


autoCompletePoints : Char -> Int
autoCompletePoints c =
    case c of
        ')' ->
            1

        ']' ->
            2

        '}' ->
            3

        '>' ->
            4

        _ ->
            Debug.todo ""
