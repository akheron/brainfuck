module Eval exposing (eval)

import Program exposing (Statement(..))
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


eval : String -> List Int -> List Int
eval code stdin =
    let
        stmts =
            Program.parse code

        finalState =
            program stmts (initialState stdin)
    in
        List.reverse finalState.stdout


program : List Statement -> EvalState -> EvalState
program program state =
    List.foldl statement state program


statement : Statement -> EvalState -> EvalState
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

        While body ->
            while body



tape : (Tape -> Tape) -> EvalState -> EvalState
tape fn state =
    { state | tape = fn state.tape }


output : EvalState -> EvalState
output state =
    { state | stdout = Tape.get state.tape :: state.stdout }


input : EvalState -> EvalState
input state =
    case state.stdin of
        head :: tail ->
            { state
                | stdin = tail
                , tape = Tape.set head state.tape
            }

        [] ->
            -- stdin at eof, no change
            state


while : List Statement -> EvalState -> EvalState
while body state =
    if Tape.get state.tape == 0 then
        state
    else
        let
            nextState =
                program body state
        in
            while body nextState
