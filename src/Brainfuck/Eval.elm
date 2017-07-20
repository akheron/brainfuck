module Brainfuck.Eval exposing (Error(InfiniteLoop, TapeOverflow), eval)

import Brainfuck.Parser as Parser exposing (Statement(..))
import Brainfuck.Tape as Tape exposing (Tape)


type alias EvalState =
    { tape : Tape
    , stdin : List Int
    , stdout : List Int
    }


type Error
    = InfiniteLoop
    | TapeOverflow


type alias EvalResult =
    Result Error EvalState


initialState : List Int -> EvalState
initialState stdin =
    { tape = Tape.empty
    , stdin = stdin
    , stdout = []
    }


eval : String -> List Int -> Result Error (List Int)
eval code stdin =
    let
        stmts =
            Parser.parse code

        extractStdout state =
            List.reverse state.stdout
    in
        program stmts (initialState stdin)
            |> Result.map extractStdout


program : List Statement -> EvalState -> EvalResult
program prog state =
    case prog of
        [] ->
            Ok state

        stmt :: tail ->
            case statement stmt state of
                Ok nextState ->
                    program tail nextState

                Err err ->
                    Err err


statement : Statement -> EvalState -> EvalResult
statement stmt =
    case stmt of
        Left ->
            moveTape Tape.left

        Right ->
            moveTape Tape.right

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


moveTape : (Tape -> Maybe Tape) -> EvalState -> EvalResult
moveTape fn state =
    case fn state.tape of
        Just newTape ->
            Ok { state | tape = newTape }

        Nothing ->
            Err TapeOverflow


tape : (Tape -> Tape) -> EvalState -> EvalResult
tape fn state =
    Ok { state | tape = fn state.tape }


output : EvalState -> EvalResult
output state =
    Ok { state | stdout = Tape.get state.tape :: state.stdout }


input : EvalState -> EvalResult
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


loop : List Statement -> EvalState -> EvalResult
loop body state =
    if Tape.get state.tape == 0 then
        Ok state
    else
        case program body state of
            Ok nextState ->
                if nextState /= state then
                    loop body nextState
                else
                    Err InfiniteLoop

            Err message ->
                Err message
