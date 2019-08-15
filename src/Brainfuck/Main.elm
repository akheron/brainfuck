module Brainfuck.Main exposing (init, main, stdoutFromResult, subsciptions, update)

import Brainfuck.Eval as Eval
import Brainfuck.Types exposing (Model, Msg(..), Stdout(..))
import Brainfuck.View exposing (view)
import Browser
import Char
import Html


init : () -> ( Model, Cmd Msg )
init _ =
    ( { code = ""
      , stdin = ""
      , stdout = Empty
      , generation = 0
      }
    , Cmd.none
    )


stdoutFromResult : Result Eval.Error (List Int) -> Stdout
stdoutFromResult result =
    case result of
        Ok output ->
            Success output

        Err Eval.InfiniteLoop ->
            Error "Infinite loop detected"

        Err Eval.TapeOverflow ->
            Error "Memory bounds exceeded"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Code code ->
            ( { model | code = code }, Cmd.none )

        Stdin stdin ->
            ( { model | stdin = stdin }, Cmd.none )

        Run ->
            let
                stdin =
                    model.stdin |> String.toList |> List.map Char.toCode

                stdout =
                    stdoutFromResult <| Eval.eval model.code stdin
            in
            ( { model | stdout = stdout }, Cmd.none )

        ShowExample example ->
            ( { model
                | code = example.code
                , stdin = example.stdin
                , stdout = Empty
                , generation = model.generation + 1
              }
            , Cmd.none
            )


subsciptions : Model -> Sub msg
subsciptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subsciptions
        }
