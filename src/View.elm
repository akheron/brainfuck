module View exposing (view)

import Char
import Html exposing (Html)
import Html.Attributes as Html
import Html.Events as Html
import Types exposing (Model, Msg(Code, Stdin, ShowExample, Run), Stdout(Empty, Success, Error))
import Examples exposing (examples)


view : Model -> Html Msg
view { code, stdin, stdout } =
    Html.section []
        [ Html.h1 [] [ Html.text "Brainfuck interpreter" ]
        , viewCode
        , viewStdin
        , viewControls
        , viewOutput stdout
        , viewExamples
        , Html.p []
            [ Html.text "Unlimited unsigned 8-bit cells with overflow. "
            , Html.text "Reading EOF leaves the cell as-is. "
            , Html.text "The interpreter was written in Elm and runs "
            , Html.text "in the browser, see "
            , Html.a
                [ Html.href "https://github.com/akheron/brainfuck" ]
                [ Html.text "source" ]
            , Html.text ". Examples adapted from "
            , Html.a
                [ Html.href "https://esolangs.org/wiki/brainfuck" ]
                [ Html.text "esolangs.org" ]
            , Html.text " and "
            , Html.a
                [ Html.href "http://brainfuck.org" ]
                [ Html.text "brainfuck.org" ]
            , Html.text "."
            ]
        ]


viewCode : Html Msg
viewCode =
    Html.div [ Html.class "code" ]
        [ Html.textarea
            [ Html.id "code"
            , Html.onInput Code
            , Html.placeholder "code"
            ]
            []
        ]


viewStdin : Html Msg
viewStdin =
    Html.div [ Html.class "stdin" ]
        [ Html.textarea
            [ Html.id "stdin"
            , Html.onInput Stdin
            , Html.placeholder "stdin"
            ]
            []
        ]


viewControls : Html Msg
viewControls =
    Html.div [ Html.class "controls" ]
        [ Html.button [ Html.onClick Run ] [ Html.text "Run" ] ]


output2html : List Int -> List (Html msg)
output2html output =
    let
        newline : Int
        newline =
            Char.toCode '\n'

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


viewOutput : Stdout -> Html Msg
viewOutput stdout =
    let
        output children =
            Html.div [ Html.class "output" ] children
    in
        case stdout of
            Empty ->
                Html.div [] []

            Success stdout ->
                output
                    [ Html.div [ Html.class "heading" ] [ Html.text "Output" ]
                    , Html.div [ Html.class "output" ] (output2html stdout)
                    ]

            Error message ->
                output
                    [ Html.div [ Html.class "error" ]
                        [ Html.text <| "Error: " ++ message ]
                    ]


viewExamples : Html Msg
viewExamples =
    let
        viewExample example =
            Html.a
                [ Html.href "#", Html.onClick (ShowExample example) ]
                [ Html.text example.name ]

        list =
            List.map viewExample examples
                |> List.intersperse (Html.text ", ")
    in
        Html.div [] (Html.text "Examples: " :: list)
