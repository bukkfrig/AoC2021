module Main exposing (..)


solve str =
    str
        |> (parse >> orElseCrash "Parse error")
        |> (evaluate >> orElseCrash "Evaluate error")


evaluate : Packet -> Maybe Int
evaluate packet =
    case packet of
        Literal _ x ->
            Just x

        Operator _ Sum xs ->
            List.foldl (\x acc -> evaluate x |> Maybe.map2 (+) acc) (Just 0) xs

        Operator _ Product xs ->
            List.foldl (\x acc -> evaluate x |> Maybe.map2 (*) acc) (Just 1) xs

        Operator _ Minimum xs ->
            List.filterMap evaluate xs |> List.minimum

        Operator _ Maximum xs ->
            List.filterMap evaluate xs |> List.maximum

        Operator _ Greater xs ->
            case List.map evaluate xs of
                [ Just x1, Just x2 ] ->
                    Just (evaluateBool (x1 > x2))

                _ ->
                    Nothing

        Operator _ Less xs ->
            case List.map evaluate xs of
                [ Just x1, Just x2 ] ->
                    Just (evaluateBool (x1 < x2))

                _ ->
                    Nothing

        Operator _ Equal xs ->
            case List.map evaluate xs of
                [ Just x1, Just x2 ] ->
                    Just (evaluateBool (x1 == x2))

                _ ->
                    Nothing


evaluateBool : Bool -> Int
evaluateBool b =
    if b then
        1

    else
        0



-- Types


type alias Version =
    Int


type Packet
    = Literal Version Int
    | Operator Version Operation (List Packet)


type Operation
    = Sum
    | Product
    | Minimum
    | Maximum
    | Greater
    | Less
    | Equal


toOperation : Int -> Maybe Operation
toOperation n =
    case n of
        0 ->
            Just Sum

        1 ->
            Just Product

        2 ->
            Just Minimum

        3 ->
            Just Maximum

        5 ->
            Just Greater

        6 ->
            Just Less

        7 ->
            Just Equal

        _ ->
            Nothing


type Length
    = BitLength Int
    | PacketCount Int


type alias Bit =
    Int


parse : String -> Maybe Packet
parse str =
    bitstream str
        |> (readPacket >> Maybe.map .value)


bitstream : String -> Bitstream
bitstream str =
    String.toList str
        |> List.filterMap hexToBits
        |> List.concat



-- Reading from Bitstream


type alias Bitstream =
    List Bit


type alias Read a =
    { value : a
    , bitsConsumed : Int
    , stream : Bitstream
    }


readPacket : Bitstream -> Maybe (Read Packet)
readPacket stream =
    let
        version =
            stream |> readVersion

        typeId =
            version |> Maybe.andThen (.stream >> readType)
    in
    typeId
        |> Maybe.andThen
            (\typeId_ ->
                if typeId_.value == 4 then
                    let
                        literal =
                            typeId |> Maybe.andThen (.stream >> readLiteral)
                    in
                    Maybe.map2 Literal
                        (Maybe.map .value version)
                        (Maybe.map .value literal)
                        |> Maybe.andThen
                            (\packet ->
                                Maybe.map4
                                    (\b1 b2 b3 stream_ ->
                                        { value = packet
                                        , bitsConsumed = b1 + b2 + b3
                                        , stream = stream_
                                        }
                                    )
                                    (Maybe.map .bitsConsumed version)
                                    (Maybe.map .bitsConsumed typeId)
                                    (Maybe.map .bitsConsumed literal)
                                    (Maybe.map .stream literal)
                            )

                else
                    let
                        length =
                            typeId |> Maybe.andThen (.stream >> readLength)
                    in
                    length
                        |> Maybe.andThen
                            (\length_ ->
                                let
                                    packets =
                                        case length_.value of
                                            BitLength n ->
                                                length |> Maybe.andThen (.stream >> readPacketsByBits n)

                                            PacketCount n ->
                                                length |> Maybe.andThen (.stream >> readPacketsByCount n)
                                in
                                Maybe.map3 Operator
                                    (Maybe.map .value version)
                                    (Maybe.andThen (.value >> toOperation) typeId)
                                    (Maybe.map .value packets)
                                    |> Maybe.andThen
                                        (\packet ->
                                            Maybe.map5
                                                (\b1 b2 b3 b4 stream_ ->
                                                    { value = packet
                                                    , bitsConsumed = b1 + b2 + b3 + b4
                                                    , stream = stream_
                                                    }
                                                )
                                                (Maybe.map .bitsConsumed version)
                                                (Maybe.map .bitsConsumed typeId)
                                                (Maybe.map .bitsConsumed length)
                                                (Maybe.map .bitsConsumed packets)
                                                (Maybe.map .stream packets)
                                        )
                            )
            )


readVersion : Bitstream -> Maybe (Read Int)
readVersion stream =
    case stream of
        a :: b :: c :: rest ->
            Just
                { bitsConsumed = 3
                , stream = rest
                , value = bitsToInt [ a, b, c ]
                }

        _ ->
            Nothing


readType : Bitstream -> Maybe (Read Int)
readType stream =
    case stream of
        a :: b :: c :: rest ->
            Just
                { bitsConsumed = 3
                , stream = rest
                , value = bitsToInt [ a, b, c ]
                }

        _ ->
            Nothing


readLiteral : Bitstream -> Maybe (Read Int)
readLiteral =
    let
        go valueBits bitsConsumed stream =
            case stream of
                0 :: a :: b :: c :: d :: rest ->
                    Just
                        { value = List.reverse (d :: c :: b :: a :: valueBits) |> bitsToInt
                        , bitsConsumed = bitsConsumed + 5
                        , stream = rest
                        }

                1 :: a :: b :: c :: d :: rest ->
                    go
                        (d :: c :: b :: a :: valueBits)
                        (bitsConsumed + 5)
                        rest

                _ ->
                    Nothing
    in
    go
        []
        0


readLength : Bitstream -> Maybe (Read Length)
readLength stream =
    case stream of
        0 :: rest ->
            case ( List.take 15 rest, List.drop 15 rest ) of
                ( valueBits, rest_ ) ->
                    if List.length valueBits /= 15 then
                        Nothing

                    else
                        Just
                            { value = BitLength (bitsToInt valueBits)
                            , bitsConsumed = 1 + 15
                            , stream = rest_
                            }

        1 :: rest ->
            case ( List.take 11 rest, List.drop 11 rest ) of
                ( valueBits, rest_ ) ->
                    if List.length valueBits /= 11 then
                        Nothing

                    else
                        Just
                            { value = PacketCount (bitsToInt valueBits)
                            , bitsConsumed = 1 + 11
                            , stream = rest_
                            }

        _ ->
            Nothing


readPacketsByBits : Int -> Bitstream -> Maybe (Read (List Packet))
readPacketsByBits bits =
    let
        go packets bitsConsumed_ stream_ =
            if bitsConsumed_ == bits then
                Just
                    { value = List.reverse packets
                    , bitsConsumed = bitsConsumed_
                    , stream = stream_
                    }

            else
                case readPacket stream_ of
                    Nothing ->
                        identity Nothing

                    Just { bitsConsumed, stream, value } ->
                        go
                            (value :: packets)
                            (bitsConsumed + bitsConsumed_)
                            stream
    in
    go
        []
        0


readPacketsByCount : Int -> Bitstream -> Maybe (Read (List Packet))
readPacketsByCount count =
    let
        go packets consumed stream_ =
            if List.length packets == count then
                Just
                    { value = List.reverse packets
                    , bitsConsumed = consumed
                    , stream = stream_
                    }

            else
                case readPacket stream_ of
                    Nothing ->
                        identity Nothing

                    Just { bitsConsumed, stream, value } ->
                        go
                            (value :: packets)
                            (bitsConsumed + consumed)
                            stream
    in
    go
        []
        0



-- Basics


bitsToInt : List Bit -> Int
bitsToInt =
    List.foldl (\bit acc -> bit + 2 * acc) 0


hexToBits : Char -> Maybe (List Bit)
hexToBits c =
    case c of
        '0' ->
            Just [ 0, 0, 0, 0 ]

        '1' ->
            Just [ 0, 0, 0, 1 ]

        '2' ->
            Just [ 0, 0, 1, 0 ]

        '3' ->
            Just [ 0, 0, 1, 1 ]

        '4' ->
            Just [ 0, 1, 0, 0 ]

        '5' ->
            Just [ 0, 1, 0, 1 ]

        '6' ->
            Just [ 0, 1, 1, 0 ]

        '7' ->
            Just [ 0, 1, 1, 1 ]

        '8' ->
            Just [ 1, 0, 0, 0 ]

        '9' ->
            Just [ 1, 0, 0, 1 ]

        'A' ->
            Just [ 1, 0, 1, 0 ]

        'B' ->
            Just [ 1, 0, 1, 1 ]

        'C' ->
            Just [ 1, 1, 0, 0 ]

        'D' ->
            Just [ 1, 1, 0, 1 ]

        'E' ->
            Just [ 1, 1, 1, 0 ]

        'F' ->
            Just [ 1, 1, 1, 1 ]

        _ ->
            Nothing


orElseCrash : String -> Maybe a -> a
orElseCrash s m =
    case m of
        Just a ->
            a

        Nothing ->
            Debug.todo s
