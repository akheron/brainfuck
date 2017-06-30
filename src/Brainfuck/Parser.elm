module Brainfuck.Parser exposing (Statement(..), parse)


type Statement
    = Left
    | Right
    | Incr
    | Decr
    | Output
    | Input
    | Loop (List Statement)


type alias ParseState =
    { body : List Statement -- body of the current scope (top-level or loop)
    , parents : List (List Statement) -- stack of outer loops
    }


emptyState : ParseState
emptyState =
    { body = []
    , parents = []
    }


{-| Parse a string to a list of statements
-}
parse : String -> List Statement
parse input =
    let
        state =
            List.foldl statement emptyState (String.toList input)

        { body } =
            unwield state
    in
        List.reverse body


{-| "Close" loops that were left open in the end (i.e. had no matching ']')
-}
unwield : ParseState -> ParseState
unwield state =
    case state.parents of
        [] ->
            state

        _ ->
            -- close unterminated loop
            unwield (endLoop state)


statement : Char -> ParseState -> ParseState
statement chr =
    case chr of
        '>' ->
            cmd Right

        '<' ->
            cmd Left

        '+' ->
            cmd Incr

        '-' ->
            cmd Decr

        '.' ->
            cmd Output

        ',' ->
            cmd Input

        '[' ->
            loop

        ']' ->
            endLoop

        _ ->
            -- no-op
            identity


cmd : Statement -> ParseState -> ParseState
cmd stmt state =
    { state | body = stmt :: state.body }


loop : ParseState -> ParseState
loop state =
    { state
        | parents = state.body :: state.parents
        , body = []
    }


endLoop : ParseState -> ParseState
endLoop state =
    case state.parents of
        head :: tail ->
            { state
                | body = Loop (List.reverse state.body) :: head
                , parents = tail
            }

        [] ->
            -- outside loop ']' is a no-op
            state
