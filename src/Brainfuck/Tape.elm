module Brainfuck.Tape exposing (Tape, decr, defaultOptions, empty, get, incr, left, right, set, withOptions)


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
        { leftCells : List Int
        , currentCell : Int
        , rightCells : List Int
        , size : Int
        , options : Options
        }


withOptions : Options -> Tape
withOptions options =
    Tape
        { leftCells = []
        , currentCell = 0
        , rightCells = []
        , size = 1
        , options = options
        }


empty : Tape
empty =
    withOptions defaultOptions


get : Tape -> Int
get (Tape tape) =
    tape.currentCell


set : Int -> Tape -> Tape
set value (Tape tape) =
    Tape { tape | currentCell = value }


right : Tape -> Maybe Tape
right (Tape tape) =
    let
        { leftCells, currentCell, rightCells, size, options } =
            tape
    in
    case rightCells of
        val :: rest ->
            Just <|
                Tape
                    { tape
                        | leftCells = currentCell :: leftCells
                        , currentCell = val
                        , rightCells = rest
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
                            | leftCells = currentCell :: leftCells
                            , currentCell = 0
                            , rightCells = []
                            , size = size + 1
                        }


left : Tape -> Maybe Tape
left (Tape tape) =
    let
        { leftCells, currentCell, rightCells } =
            tape
    in
    case leftCells of
        val :: rest ->
            Just <|
                Tape
                    { tape
                        | leftCells = rest
                        , currentCell = val
                        , rightCells = currentCell :: rightCells
                    }

        [] ->
            -- Attempt to go left from cell 0
            Nothing


incr : Tape -> Tape
incr (Tape tape) =
    let
        { currentCell, options } =
            tape

        new =
            if currentCell == options.maxValue then
                options.minValue

            else
                currentCell + 1
    in
    Tape { tape | currentCell = new }


decr : Tape -> Tape
decr (Tape tape) =
    let
        { currentCell, options } =
            tape

        new =
            if currentCell == options.minValue then
                options.maxValue

            else
                currentCell - 1
    in
    Tape { tape | currentCell = new }
