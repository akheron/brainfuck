module Program exposing (Statement(..), parse)


type Statement
    = Left
    | Right
    | Incr
    | Decr
    | Output
    | Input
    | While (List Statement)


parse : String -> List Statement
parse input =
    let
        ( _, program ) =
            parse_ 0 input []
    in
        List.reverse program


parse_ : Int -> String -> List Statement -> ( String, List Statement )
parse_ depth input program =
    let
        ( result, tail ) =
            statement depth input

        newProgram =
            case result of
                Ok stmt ->
                    stmt :: program

                _ ->
                    program
    in
        case result of
            End ->
                ( tail, newProgram )

            _ ->
                parse_ depth tail newProgram


type ParseResult
    = Ok Statement
    | NoOp
    | End


statement : Int -> String -> ( ParseResult, String )
statement depth input =
    let
        statement_ : ( Char, String ) -> ( ParseResult, String )
        statement_ ( head, tail ) =
            case head of
                '>' ->
                    ( Ok Right, tail )

                '<' ->
                    ( Ok Left, tail )

                '+' ->
                    ( Ok Incr, tail )

                '-' ->
                    ( Ok Decr, tail )

                '.' ->
                    ( Ok Output, tail )

                ',' ->
                    ( Ok Input, tail )

                '[' ->
                    let
                        ( rest, program ) =
                            parse_ (depth + 1) tail []
                    in
                        ( Ok <| While <| List.reverse program, rest )

                ']' ->
                    if depth > 0 then
                        -- Inside while, stop parsing at ]
                        ( End, tail )
                    else
                        -- Outside while, unbalanced ] is a no-op
                        ( NoOp, tail )

                _ ->
                    ( NoOp, tail )
    in
        String.uncons input
            |> Maybe.map statement_
            |> Maybe.withDefault ( End, "" )
