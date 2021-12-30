module Main exposing (..)

import Char
import List.Extra


solve str =
    parse str
        |> orElseCrash "Couldn't parse input?"
        |> magnitude


parse : String -> Maybe Snail
parse str =
    let
        snails =
            String.lines str
                |> List.map (String.toList >> readSnail >> Maybe.map .value)
    in
    case snails of
        x :: xs ->
            List.foldl (\n acc -> Maybe.map2 add acc n) x xs

        [] ->
            Nothing



-- Snail number logic


type Snail
    = R Int
    | S ( Snail, Snail )


magnitude : Snail -> Int
magnitude sn =
    case sn of
        R n ->
            n

        S ( n1, n2 ) ->
            3 * magnitude n1 + 2 * magnitude n2


toString : Snail -> String
toString sn =
    case sn of
        R n ->
            String.fromInt n

        S ( a, b ) ->
            "[" ++ toString a ++ "," ++ toString b ++ "]"


add : Snail -> Snail -> Snail
add s1 s2 =
    reduce (S ( s1, s2 ))


reduce : Snail -> Snail
reduce sn =
    case explode sn of
        exploded ->
            if sn /= exploded then
                reduce exploded

            else
                case split exploded of
                    splitted ->
                        if exploded /= splitted then
                            reduce splitted

                        else
                            splitted


explode : Snail -> Snail
explode sn =
    let
        go : ( List Char, Int, List Char ) -> Snail
        go ( prev, depth, next ) =
            case next of
                [] ->
                    sn

                a :: rest ->
                    if a == ']' then
                        go ( a :: prev, depth - 1, rest )

                    else if a == '[' && depth < 4 then
                        go ( a :: prev, depth + 1, rest )

                    else if a /= '[' then
                        go ( a :: prev, depth, rest )

                    else
                        let
                            snail =
                                readSnail next
                        in
                        case snail of
                            Just { stream, value } ->
                                case value of
                                    S ( R left, R right ) ->
                                        let
                                            prev_ =
                                                let
                                                    beforeLeftInt =
                                                        prev
                                                            |> List.Extra.takeWhile (not << Char.isDigit)

                                                    withNearest =
                                                        prev
                                                            |> List.Extra.dropWhile (not << Char.isDigit)

                                                    leftInt =
                                                        withNearest
                                                            |> List.Extra.takeWhile Char.isDigit
                                                            |> List.reverse
                                                            |> String.fromList
                                                            |> String.toInt

                                                    afterLeftInt =
                                                        withNearest |> List.Extra.dropWhile Char.isDigit
                                                in
                                                case leftInt of
                                                    Nothing ->
                                                        beforeLeftInt

                                                    Just n ->
                                                        beforeLeftInt
                                                            ++ (left + n |> String.fromInt |> String.toList |> List.reverse)
                                                            ++ afterLeftInt

                                            next_ =
                                                let
                                                    beforeRightInt =
                                                        stream
                                                            |> List.Extra.takeWhile (not << Char.isDigit)

                                                    withNearest =
                                                        stream
                                                            |> List.Extra.dropWhile (not << Char.isDigit)

                                                    rightInt =
                                                        withNearest
                                                            |> List.Extra.takeWhile Char.isDigit
                                                            |> String.fromList
                                                            |> String.toInt

                                                    afterRightInt =
                                                        withNearest |> List.Extra.dropWhile Char.isDigit
                                                in
                                                case rightInt of
                                                    Nothing ->
                                                        beforeRightInt

                                                    Just n ->
                                                        beforeRightInt
                                                            ++ (right + n |> String.fromInt |> String.toList)
                                                            ++ afterRightInt
                                        in
                                        readSnail (List.reverse prev_ ++ ('0' :: next_))
                                            |> Maybe.map .value
                                            |> orElseCrash "Couldn't read the reconstructed snail after exploding?"

                                    _ ->
                                        Debug.todo "Value at depth 4 is not a pair of regular numbers?"

                            Nothing ->
                                Debug.todo "Characters at start of depth 4 are not a snail number?"
    in
    go ( [], 0, toString sn |> String.toList )


split : Snail -> Snail
split sn =
    let
        go todo done =
            case todo of
                [] ->
                    sn

                a :: rest ->
                    case readInt todo of
                        Just { stream, value } ->
                            if value < 10 then
                                go stream ((String.fromInt value |> String.toList |> List.reverse) ++ done)

                            else
                                let
                                    left =
                                        (value // 2) |> String.fromInt |> String.toList

                                    right =
                                        ceiling (toFloat value / 2) |> String.fromInt |> String.toList
                                in
                                List.reverse done
                                    ++ ('[' :: left ++ ',' :: right ++ [ ']' ])
                                    ++ stream
                                    |> readSnail
                                    |> Maybe.map .value
                                    |> orElseCrash "Couldn't read reconstructed string after splitting?"

                        Nothing ->
                            go rest (a :: done)
    in
    go (sn |> toString |> String.toList) []



-- Reading from list of characters


type alias Read a =
    { stream : List Char, value : a }


readInt : List Char -> Maybe (Read Int)
readInt stream =
    let
        ( digits, rest ) =
            ( stream |> List.Extra.takeWhile Char.isDigit
            , stream |> List.Extra.dropWhile Char.isDigit
            )
    in
    Maybe.map (\n -> { stream = rest, value = n })
        (String.toInt (String.fromList digits))


readChar : Char -> List Char -> Maybe (Read Char)
readChar c stream =
    case stream of
        x :: rest ->
            if x == c then
                Just { stream = rest, value = c }

            else
                Nothing

        _ ->
            Nothing


readSnail : List Char -> Maybe (Read Snail)
readSnail stream =
    case stream of
        '[' :: rest ->
            let
                first =
                    rest |> readSnail

                second =
                    first
                        |> Maybe.andThen (.stream >> readChar ',')
                        |> Maybe.andThen (.stream >> readSnail)
            in
            Maybe.map3 (\x y s -> { stream = s.stream, value = S ( x, y ) })
                (Maybe.map .value first)
                (Maybe.map .value second)
                (second |> Maybe.andThen (.stream >> readChar ']'))

        _ ->
            Maybe.map (\regular -> { stream = regular.stream, value = R regular.value })
                (readInt stream)



-- Basics


orElseCrash s m =
    case m of
        Just a ->
            a

        Nothing ->
            Debug.todo s
