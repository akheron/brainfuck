module Brainfuck.Tape exposing (Tape, withOptions, defaultOptions, empty, get, set, right, left, incr, decr)


type alias Options =
    { minValue : Int
    , maxValue : Int
    , maxSize : Int
    }


defaultOptions : Options
defaultOptions =
    { minValue = 0
    , maxValue = 255
    , maxSize = 30000
    }


type Tape
    = Tape
        { left : List Int
        , current : Int
        , right : List Int
        , size : Int
        , options : Options
        }


withOptions : Options -> Tape
withOptions options =
    Tape { left = [], current = 0, right = [], size = 1, options = options }


empty : Tape
empty =
    withOptions defaultOptions


get : Tape -> Int
get (Tape tape) =
    tape.current


set : Int -> Tape -> Tape
set value (Tape tape) =
    Tape { tape | current = value }


right : Tape -> Maybe Tape
right (Tape tape) =
    let
        { left, current, right, size, options } =
            tape
    in
        case right of
            val :: rest ->
                Just <|
                    Tape
                        { tape
                            | left = current :: left
                            , current = val
                            , right = rest
                        }

            [] ->
                if size >= options.maxSize then
                    -- Maximum tape size reached
                    Nothing
                else
                    -- Grow tape to the right
                    Just <|
                        Tape
                            { tape
                                | left = current :: left
                                , current = 0
                                , right = []
                                , size = size + 1
                            }


left : Tape -> Maybe Tape
left (Tape tape) =
    let
        { left, current, right } =
            tape
    in
        case left of
            val :: rest ->
                Just <|
                    Tape
                        { tape
                            | left = rest
                            , current = val
                            , right = current :: right
                        }

            [] ->
                -- Attempt to go left from cell 0
                Nothing


incr : Tape -> Tape
incr (Tape tape) =
    let
        { current, options } =
            tape

        new =
            if current == options.maxValue then
                options.minValue
            else
                current + 1
    in
        Tape { tape | current = new }


decr : Tape -> Tape
decr (Tape tape) =
    let
        { current, options } =
            tape

        new =
            if current == options.minValue then
                options.maxValue
            else
                current - 1
    in
        Tape { tape | current = new }
