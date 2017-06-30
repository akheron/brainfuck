module Brainfuck.Main exposing (..)

import Char
import Html
import Brainfuck.Eval as Eval
import Brainfuck.View exposing (view)
import Brainfuck.Types exposing (Model, Msg(Code, Stdin, ShowExample, Run), Stdout(Empty, Success, Error))


(=>) : Model -> Cmd msg -> ( Model, Cmd msg )
(=>) model msg =
    ( model, msg )


init : ( Model, Cmd Msg )
init =
    { code = ""
    , stdin = ""
    , stdout = Empty
    , generation = 0
    }
        => Cmd.none


stdoutFromResult : Result Eval.Error (List Int) -> Stdout
stdoutFromResult result =
    case result of
        Ok output ->
            Success output

        Err Eval.InfiniteLoop ->
            Error "Infinite loop detected"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Code code ->
            { model | code = code } => Cmd.none

        Stdin stdin ->
            { model | stdin = stdin } => Cmd.none

        Run ->
            let
                stdin =
                    model.stdin |> String.toList |> List.map Char.toCode

                stdout =
                    stdoutFromResult <| Eval.eval model.code stdin
            in
                { model | stdout = stdout } => Cmd.none

        ShowExample example ->
            { model
                | code = example.code
                , stdin = example.stdin
                , stdout = Empty
                , generation = model.generation + 1
            }
                => Cmd.none


subsciptions : Model -> Sub msg
subsciptions model =
    Sub.none


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subsciptions
        }
