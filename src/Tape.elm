module Tape exposing (Tape, empty, get, set, right, left, incr, decr)


cellBounds =
    { min = 0
    , max = 255
    }


type Tape
    = Tape
        { left : List Int
        , current : Int
        , right : List Int
        }


empty : Tape
empty =
    Tape { left = [], current = 0, right = [] }


get : Tape -> Int
get (Tape tape) =
    tape.current


set : Int -> Tape -> Tape
set value (Tape tape) =
    Tape { tape | current = value }


right : Tape -> Tape
right (Tape { left, current, right }) =
    case right of
        val :: rest ->
            Tape { left = current :: left, current = val, right = rest }

        [] ->
            Tape { left = current :: left, current = 0, right = [] }


left : Tape -> Tape
left (Tape { left, current, right }) =
    case left of
        val :: rest ->
            Tape { left = rest, current = val, right = current :: right }

        [] ->
            -- At cell 0
            Tape { left = left, current = current, right = right }


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
