module Brainfuck.Tape exposing (Tape, empty, get, set, right, left, incr, decr)


cellBounds =
    { min = 0
    , max = 255
    }


tapeSize =
    30000


type Tape
    = Tape
        { left : List Int
        , current : Int
        , right : List Int
        , cellCount : Int
        }


empty : Tape
empty =
    Tape { left = [], current = 0, right = [], cellCount = 1 }


get : Tape -> Int
get (Tape tape) =
    tape.current


set : Int -> Tape -> Tape
set value (Tape tape) =
    Tape { tape | current = value }


right : Tape -> Maybe Tape
right (Tape tape) =
    let
        { left, current, right, cellCount } =
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
                if cellCount >= tapeSize then
                    -- Maximum tape size reached
                    Nothing
                else
                    -- Grow tape to the right
                    Just <|
                        Tape
                            { left = current :: left
                            , current = 0
                            , right = []
                            , cellCount = cellCount + 1
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
        new =
            if tape.current == cellBounds.max then
                cellBounds.min
            else
                tape.current + 1
    in
        Tape { tape | current = new }


decr : Tape -> Tape
decr (Tape tape) =
    let
        new =
            if tape.current == cellBounds.min then
                cellBounds.max
            else
                tape.current - 1
    in
        Tape { tape | current = new }
