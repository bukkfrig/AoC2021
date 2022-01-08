module Main exposing (solve)

import Array exposing (Array)
import Parser exposing ((|=), Parser)


solve str =
    let
        snails =
            parse str
                |> List.map (okOrCrash "Couldn't parse a snail number?")
    in
    List.foldl
        (\this max ->
            List.concat
                [ List.map (\other -> add this other |> magnitude) snails
                , List.map (\other -> add other this |> magnitude) snails
                , List.singleton max
                ]
                |> (List.maximum >> orElseCrash "No maximum?")
                |> Debug.log "Maximum so far"
        )
        0
        snails


type alias Tokens =
    Array Token


type alias DeadEnds =
    List Parser.DeadEnd


parse : String -> List (Result DeadEnds Tokens)
parse str =
    String.lines str
        |> List.map (Parser.run tokenParser)



-- Tokens


type Token
    = Open
    | Close
    | Regular Int


tokenParser : Parser Tokens
tokenParser =
    Parser.loop []
        (\tokens ->
            Parser.oneOf
                [ Parser.succeed (\() -> Parser.Loop (Open :: tokens))
                    |= Parser.symbol "["
                , Parser.succeed (\() -> Parser.Loop (Close :: tokens))
                    |= Parser.symbol "]"
                , Parser.succeed (\() -> Parser.Loop tokens)
                    |= Parser.symbol ","
                , Parser.succeed (\token -> Parser.Loop (Regular token :: tokens))
                    |= Parser.int
                , Parser.succeed (\() -> Parser.Done (Array.fromList (List.reverse tokens)))
                    |= Parser.end
                ]
        )


isRegular : Token -> Bool
isRegular token =
    case token of
        Regular _ ->
            True

        _ ->
            False



-- Snail number logic


add : Tokens -> Tokens -> Tokens
add s1 s2 =
    reduce
        (Array.empty
            |> Array.push Open
            |> (\stream -> Array.append stream s1)
            |> (\stream -> Array.append stream s2)
            |> Array.push Close
        )


reduce : Tokens -> Tokens
reduce sn =
    case explode sn of
        Modified tokens ->
            reduce tokens

        NotModified ->
            case split sn of
                Modified tokens ->
                    reduce tokens

                NotModified ->
                    sn


{-| Replace something like:

        [ ..., Open, Regular 1, Regular 2, Close, ... ]

    with

        [ ..., Regular 0, ... ]

-}
deletePairStartingAt : Int -> Tokens -> Tokens
deletePairStartingAt n tokens =
    let
        length =
            Array.length tokens

        ( left, right ) =
            ( Array.slice 0 (n - 1) tokens
            , Array.slice (n + 3) length tokens
            )
    in
    Array.empty
        |> (\stream -> Array.append stream left)
        |> Array.push (Regular 0)
        |> (\stream -> Array.append stream right)


explode : Tokens -> Modification Tokens
explode tokens =
    let
        go depth index leftNeighbour =
            if depth > 4 then
                Modified <|
                    case
                        ( tokens |> Array.get index
                        , tokens |> Array.get (index + 1)
                        )
                    of
                        ( Just (Regular n1), Just (Regular n2) ) ->
                            let
                                rightNeighbour =
                                    let
                                        length =
                                            tokens |> Array.length

                                        startOfUnvisited =
                                            index + 3

                                        unvisited =
                                            tokens |> Array.slice startOfUnvisited length
                                    in
                                    case arrayFind isRegular unvisited of
                                        Just ( ix, Regular n ) ->
                                            Just ( startOfUnvisited + ix, n )

                                        _ ->
                                            Nothing

                                updateNeighbours =
                                    case ( leftNeighbour, rightNeighbour ) of
                                        ( Nothing, Nothing ) ->
                                            identity

                                        ( Just ( leftIndex, l1 ), Nothing ) ->
                                            Array.set leftIndex (Regular (l1 + n1))

                                        ( Nothing, Just ( rightIndex, r1 ) ) ->
                                            Array.set rightIndex (Regular (n2 + r1))

                                        ( Just ( leftIndex, l1 ), Just ( rightIndex, r1 ) ) ->
                                            identity
                                                >> Array.set leftIndex (Regular (l1 + n1))
                                                >> Array.set rightIndex (Regular (n2 + r1))
                            in
                            tokens
                                |> updateNeighbours
                                |> deletePairStartingAt index

                        _ ->
                            Debug.todo ("Not a pair at depth 4 with index " ++ Debug.toString index)

            else
                case tokens |> Array.get index of
                    Nothing ->
                        NotModified

                    Just Open ->
                        go
                            (depth + 1)
                            (index + 1)
                            leftNeighbour

                    Just Close ->
                        go
                            (depth - 1)
                            (index + 1)
                            leftNeighbour

                    Just (Regular n1) ->
                        go
                            depth
                            (index + 1)
                            (Just ( index, n1 ))
    in
    go
        0
        0
        Nothing


split : Tokens -> Modification Tokens
split tokens =
    let
        go index =
            case Array.get index tokens of
                Just (Regular n) ->
                    if n >= 10 then
                        Modified <|
                            let
                                length =
                                    tokens |> Array.length

                                ( left, right ) =
                                    ( tokens |> Array.slice 0 index
                                    , tokens |> Array.slice (index + 1) length
                                    )
                            in
                            Array.empty
                                |> (\stream -> Array.append stream left)
                                |> Array.push Open
                                |> Array.push (Regular (n // 2))
                                |> Array.push (Regular (ceiling (toFloat n / 2)))
                                |> Array.push Close
                                |> (\stream -> Array.append stream right)

                    else
                        go
                            (index + 1)

                Nothing ->
                    NotModified

                Just _ ->
                    go
                        (index + 1)
    in
    go
        0


magnitude : Tokens -> Int
magnitude tokens =
    let
        go index stack =
            case Array.get index tokens of
                Just Open ->
                    -- Push an empty stack frame
                    go
                        (index + 1)
                        (( Nothing, Nothing ) :: stack)

                Just (Regular n) ->
                    -- Fill in the frame at the top of the stack. Does it need it's first or second number?
                    case stack of
                        [] ->
                            n

                        ( Nothing, Nothing ) :: xs ->
                            go
                                (index + 1)
                                (( Just n, Nothing ) :: xs)

                        ( Just n1, Nothing ) :: xs ->
                            go
                                (index + 1)
                                (( Just n1, Just n ) :: xs)

                        _ ->
                            Debug.todo "This stack frame already complete?"

                Just Close ->
                    -- Pop the top stack frame , turn it into a magnitude, then put it into the next frame of the stack.
                    case stack of
                        ( Just n1, Just n2 ) :: xs ->
                            let
                                mag =
                                    3 * n1 + 2 * n2
                            in
                            case xs of
                                [] ->
                                    mag

                                ( Nothing, Nothing ) :: rest ->
                                    go
                                        (index + 1)
                                        (( Just mag, Nothing ) :: rest)

                                ( Just m1, Nothing ) :: rest ->
                                    go
                                        (index + 1)
                                        (( Just m1, Just mag ) :: rest)

                                _ ->
                                    Debug.todo "Next stack frame already complete?"

                        [] ->
                            Debug.todo "Empty stack?"

                        _ ->
                            Debug.todo "Incomplete stack frame?"

                Nothing ->
                    Debug.todo "Stack didn't empty before end of tokens?"
    in
    go
        0
        []



-- Basics


type Modification a
    = Modified a
    | NotModified


okOrCrash : String -> Result error a -> a
okOrCrash reason result =
    case result of
        Ok a ->
            a

        Err error ->
            Debug.todo (reason ++ "\n" ++ Debug.toString error)


orElseCrash : String -> Maybe a -> a
orElseCrash reason maybe =
    case maybe of
        Just a ->
            a

        Nothing ->
            Debug.todo reason


arrayFind : (a -> Bool) -> Array a -> Maybe ( Int, a )
arrayFind f xs =
    let
        go n =
            case Array.get n xs of
                Just x ->
                    if f x then
                        Just ( n, x )

                    else
                        go
                            (n + 1)

                Nothing ->
                    Nothing
    in
    go
        0
