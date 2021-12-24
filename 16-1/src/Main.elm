module Main exposing (..)


solve str =
    versionSum (parse str |> orElseCrash "Parse error")


parse : String -> Maybe Packet
parse str =
    bitstream str |> readPacket |> Maybe.map .value


bitstream str =
    String.toList str
        |> List.filterMap hexToBits
        |> List.concatMap identity


versionSum : Packet -> Int
versionSum packet =
    case packet of
        Literal version _ ->
            version

        Operator version _ packets ->
            List.foldl (\packet_ acc -> versionSum packet_ + acc) version packets



-- Types


type alias Version =
    Int


type Packet
    = Literal Version Int
    | Operator Version Operation (List Packet)


type Operation
    = SomeOperation


toOperation : Int -> Maybe Operation
toOperation =
    always (Just SomeOperation)


type Length
    = BitLength Int
    | PacketCount Int


type alias Bit =
    Int



-- Reading from Bitstream


type alias Bitstream =
    List Bit


type alias Read a =
    { bitsConsumed : Int, stream : Bitstream, value : a }


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
            (\{ value } ->
                if value == 4 then
                    let
                        literal =
                            typeId |> Maybe.andThen (.stream >> readLiteralValue)
                    in
                    Maybe.map2 Literal
                        (Maybe.map .value version)
                        (Maybe.map .value literal)
                        |> Maybe.andThen
                            (\packet ->
                                Maybe.map4 (\b1 b2 b3 stream_ -> { bitsConsumed = b1 + b2 + b3, stream = stream_, value = packet })
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
                                                length |> Maybe.andThen (.stream >> readPacketsBits n)

                                            PacketCount n ->
                                                length |> Maybe.andThen (.stream >> readPacketsCount n)
                                in
                                Maybe.map3 Operator
                                    (Maybe.map .value version)
                                    (Maybe.andThen (.value >> toOperation) typeId)
                                    (Maybe.map .value packets)
                                    |> Maybe.andThen
                                        (\packet ->
                                            Maybe.map5 (\b1 b2 b3 b4 stream_ -> { bitsConsumed = b1 + b2 + b3 + b4, stream = stream_, value = packet })
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


readLiteralValue : Bitstream -> Maybe (Read Int)
readLiteralValue stream =
    let
        go remaining ( valueBits, bitsConsumed ) =
            case remaining of
                1 :: a :: b :: c :: d :: rest ->
                    go rest ( valueBits ++ [ a, b, c, d ], bitsConsumed + 5 )

                0 :: a :: b :: c :: d :: rest ->
                    Just
                        { bitsConsumed = bitsConsumed + 5
                        , stream = rest
                        , value = bitsToInt (valueBits ++ [ a, b, c, d ])
                        }

                _ ->
                    Nothing
    in
    go stream ( [], 0 )


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
                            { bitsConsumed = 1 + 15
                            , stream = rest_
                            , value = BitLength (bitsToInt valueBits)
                            }

        1 :: rest ->
            case ( List.take 11 rest, List.drop 11 rest ) of
                ( valueBits, rest_ ) ->
                    if List.length valueBits /= 11 then
                        Nothing

                    else
                        Just
                            { bitsConsumed = 1 + 11
                            , stream = rest_
                            , value = PacketCount (bitsToInt valueBits)
                            }

        _ ->
            Nothing


readPacketsBits : Int -> Bitstream -> Maybe (Read (List Packet))
readPacketsBits n inStream =
    let
        go : List Packet -> Int -> Bitstream -> Maybe (Read (List Packet))
        go packets consumed stream_ =
            case ( stream_ |> readPacket, () ) of
                ( Just { bitsConsumed, stream, value }, () ) ->
                    if bitsConsumed + consumed == n then
                        Just
                            { bitsConsumed = bitsConsumed + consumed
                            , stream = stream
                            , value = packets ++ [ value ]
                            }

                    else
                        go
                            (packets ++ [ value ])
                            (bitsConsumed + consumed)
                            stream

                ( Nothing, () ) ->
                    Nothing
    in
    go [] 0 inStream


readPacketsCount : Int -> Bitstream -> Maybe (Read (List Packet))
readPacketsCount n inStream =
    let
        go : List Packet -> Int -> Bitstream -> Maybe (Read (List Packet))
        go packets consumed stream_ =
            case ( stream_ |> readPacket, () ) of
                ( Just { bitsConsumed, stream, value }, () ) ->
                    if List.length packets + 1 == n then
                        Just
                            { bitsConsumed = bitsConsumed + consumed
                            , stream = stream
                            , value = packets ++ [ value ]
                            }

                    else
                        go
                            (packets ++ [ value ])
                            (bitsConsumed + consumed)
                            stream

                ( Nothing, () ) ->
                    Nothing
    in
    go [] 0 inStream



-- Basics


bitsToInt : List Int -> Int
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
