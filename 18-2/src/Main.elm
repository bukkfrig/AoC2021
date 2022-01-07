module Main exposing (solve)

import Array exposing (Array)
import Parser exposing ((|.), (|=), Parser)


solve str =
    let
        snails : List (Array Token)
        snails =
            List.map (okOrCrash "Couldn't parse a snail number?") (parse str)
    in
    List.foldl
        (\first best ->
            List.maximum (best :: List.map (add first >> magnitude) snails ++ List.map (\second -> add second first |> magnitude) snails)
                |> orElseCrash "No maximum?"
                |> Debug.log "Maximum so far: "
        )
        0
        snails


parse : String -> List (Result (List Parser.DeadEnd) (Array Token))
parse str =
    String.lines str
        |> List.map (Parser.run parseTokens)


parseTokens : Parser (Array Token)
parseTokens =
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



-- Snail number logic


type alias Tokens =
    Array Token


type Token
    = Open
    | Close
    | Regular Int


magnitude : Tokens -> Int
magnitude sn =
    magnitudeHelp 0 [] sn


magnitudeHelp index stack tokens =
    case Array.get index tokens of
        Just (Regular n2) ->
            case stack of
                [] ->
                    n2

                ( Nothing, Nothing ) :: xs ->
                    magnitudeHelp (index + 1) (( Just n2, Nothing ) :: xs) tokens

                ( Just n1, Nothing ) :: xs ->
                    magnitudeHelp (index + 1) (( Just n1, Just n2 ) :: xs) tokens

                _ ->
                    Debug.todo "This stack frame already complete?"

        Just Open ->
            magnitudeHelp (index + 1) (( Nothing, Nothing ) :: stack) tokens

        Just Close ->
            case stack of
                [] ->
                    Debug.todo "Empty stack?"

                ( Just n1, Just n2 ) :: xs ->
                    let
                        mag =
                            3 * n1 + 2 * n2
                    in
                    case xs of
                        [] ->
                            mag

                        ( Nothing, Nothing ) :: rest ->
                            magnitudeHelp (index + 1) (( Just mag, Nothing ) :: rest) tokens

                        ( Just m1, Nothing ) :: rest ->
                            magnitudeHelp (index + 1) (( Just m1, Just mag ) :: rest) tokens

                        _ ->
                            Debug.todo "Next stack frame already complete?"

                _ ->
                    Debug.todo "Incomplete stack frame?"

        Nothing ->
            Debug.todo "Stack didn't empty before end of tokens?"


add : Tokens -> Tokens -> Tokens
add s1 s2 =
    reduce
        (Array.empty
            |> Array.push Open
            |> (\tokens -> Array.append tokens s1)
            |> (\tokens -> Array.append tokens s2)
            |> Array.push Close
        )


reduce : Tokens -> Tokens
reduce sn =
    case explode sn of
        Exploded tokens ->
            reduce tokens

        NotExploded ->
            case split sn of
                Split tokens ->
                    reduce tokens

                NotSplit ->
                    sn


type ExplodeResult
    = Exploded Tokens
    | NotExploded


type SplitResult
    = Split Tokens
    | NotSplit


deletePairStartingAt : Int -> Tokens -> Tokens
deletePairStartingAt n tokens =
    tokens
        |> Array.slice 0 (n - 1)
        |> Array.push (Regular 0)
        |> (\t -> Array.append t (Array.slice (n + 3) (Array.length tokens) tokens))


explode : Tokens -> ExplodeResult
explode sn =
    explodeHelp 0 0 Nothing sn


explodeHelp : Int -> Int -> Maybe ( Int, Int ) -> Tokens -> ExplodeResult
explodeHelp index depth lastRegular sn =
    if depth > 4 then
        case ( Array.get index sn, Array.get (index + 1) sn ) of
            ( Just (Regular n1), Just (Regular n2) ) ->
                (case ( lastRegular, arrayFind isRegular (Array.slice (index + 3) (Array.length sn) sn) ) of
                    ( Just ( li, l1 ), Just ( ri, Regular r1 ) ) ->
                        sn |> Array.set li (Regular (l1 + n1)) |> Array.set (ri + index + 3) (Regular (r1 + n2))

                    ( Just ( li, l1 ), Nothing ) ->
                        Array.set li (Regular (l1 + n1)) sn

                    ( Nothing, Just ( ri, Regular r1 ) ) ->
                        Array.set (ri + index + 3) (Regular (r1 + n2)) sn

                    ( Nothing, Nothing ) ->
                        sn

                    _ ->
                        Debug.todo "Found something that wasn't regular?"
                )
                    |> deletePairStartingAt index
                    |> Exploded

            _ ->
                Debug.todo ("Not a pair at depth 4 with index " ++ Debug.toString index)

    else
        case Array.get index sn of
            Nothing ->
                NotExploded

            Just Open ->
                explodeHelp (index + 1) (depth + 1) lastRegular sn

            Just Close ->
                explodeHelp (index + 1) (depth - 1) lastRegular sn

            Just (Regular n1) ->
                explodeHelp (index + 1) depth (Just ( index, n1 )) sn


isRegular : Token -> Bool
isRegular token =
    case token of
        Regular _ ->
            True

        _ ->
            False


arrayFind : (a -> Bool) -> Array a -> Maybe ( Int, a )
arrayFind f xs =
    arrayFindHelp 0 f xs


arrayFindHelp : Int -> (a -> Bool) -> Array a -> Maybe ( Int, a )
arrayFindHelp n f xs =
    case Array.get n xs of
        Just x ->
            if f x then
                Just ( n, x )

            else
                arrayFindHelp (n + 1) f xs

        Nothing ->
            Nothing


split : Tokens -> SplitResult
split sn =
    splitHelp 0 sn


splitHelp : Int -> Tokens -> SplitResult
splitHelp index sn =
    case Array.get index sn of
        Nothing ->
            NotSplit

        Just (Regular n) ->
            if n < 10 then
                splitHelp (index + 1) sn

            else
                sn
                    |> Array.slice 0 index
                    |> Array.push Open
                    |> Array.push (Regular (n // 2))
                    |> Array.push (Regular (ceiling (toFloat n / 2)))
                    |> Array.push Close
                    |> (\t -> Array.append t (Array.slice (index + 1) (Array.length sn) sn))
                    |> Split

        Just _ ->
            splitHelp (index + 1) sn



-- Basics


orElseCrash s m =
    case m of
        Just a ->
            a

        Nothing ->
            Debug.todo s


okOrCrash : String -> Result e a -> a
okOrCrash s r =
    case r of
        Ok a ->
            a

        Err e ->
            Debug.todo (s ++ "\n" ++ Debug.toString e)
