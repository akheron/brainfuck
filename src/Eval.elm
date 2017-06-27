module Eval exposing (eval)

import Parser exposing (Statement(..))
import Tape exposing (Tape)


type alias EvalState =
    { tape : Tape
    , stdin : List Int
    , stdout : List Int
    }


initialState : List Int -> EvalState
initialState stdin =
    { tape = Tape.empty
    , stdin = stdin
    , stdout = []
    }


eval : String -> List Int -> Result String (List Int)
eval code stdin =
    let
        stmts =
            Parser.parse code

        extractStdout state =
            List.reverse state.stdout
    in
        program stmts (initialState stdin)
            |> Result.map extractStdout


program : List Statement -> EvalState -> Result String EvalState
program prog state =
    case prog of
        [] ->
            Ok state

        stmt :: tail ->
            case statement stmt state of
                Ok nextState ->
                    program tail nextState

                Err message ->
                    Err message


statement : Statement -> EvalState -> Result String EvalState
statement stmt =
    case stmt of
        Left ->
            tape Tape.left

        Right ->
            tape Tape.right

        Incr ->
            tape Tape.incr

        Decr ->
            tape Tape.decr

        Output ->
            output

        Input ->
            input

        Loop body ->
            loop body


tape : (Tape -> Tape) -> EvalState -> Result String EvalState
tape fn state =
    Ok { state | tape = fn state.tape }


output : EvalState -> Result String EvalState
output state =
    Ok { state | stdout = Tape.get state.tape :: state.stdout }


input : EvalState -> Result String EvalState
input state =
    Ok <|
        case state.stdin of
            head :: tail ->
                { state
                    | stdin = tail
                    , tape = Tape.set head state.tape
                }

            [] ->
                -- stdin at eof, no change
                state


loop : List Statement -> EvalState -> Result String EvalState
loop body state =
    if Tape.get state.tape == 0 then
        Ok state
    else
        case program body state of
            Ok nextState ->
                if nextState /= state then
                    loop body nextState
                else
                    Err "Infinite loop detected"

            Err message ->
                Err message
