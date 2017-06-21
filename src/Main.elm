port module Main exposing (main)

import Char
import Html exposing (Html)
import Eval
import View exposing (view)
import Types exposing (Model, Msg(Code, Stdin, ShowExample, Run))


port setValue : ( String, String ) -> Cmd msg


(=>) : Model -> Cmd msg -> ( Model, Cmd msg )
(=>) model msg =
    ( model, msg )


init : ( Model, Cmd Msg )
init =
    { code = ""
    , stdin = ""
    , stdout = []
    }
        => Cmd.none


newline : Int
newline =
    Char.toCode '\n'


output2html : List Int -> List (Html msg)
output2html output =
    let
        trans : Int -> Html msg
        trans n =
            if n == newline then
                Html.br [] []
            else if n < 32 || n > 127 then
                -- non-printable
                Html.code [] [ Html.text <| "<" ++ (toString n) ++ ">" ]
            else
                -- printable
                n |> Char.fromCode |> String.fromChar |> Html.text
    in
        List.map trans output


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

                output =
                    Eval.eval model.code stdin

                stdout =
                    output2html output
            in
                { model | stdout = stdout } => Cmd.none

        ShowExample example ->
            { model | code = example.code, stdin = example.stdin }
                => Cmd.batch
                    [ setValue ( "code", example.code )
                    , setValue ( "stdin", example.stdin )
                    ]


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
