module Main exposing (main)

import Browser
import Html exposing (Html, div, text, textarea)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onInput)
import Json.Encode as E
import Lisa
import Lisa.Parser
import Lisa.Process


monodiv txt =
    div
        [ style "font-family" "monospace"
        , style "white-space" "pre"
        ]
        [ text txt ]


view : String -> Html String
view input =
    div []
        [ textarea
            [ onInput identity
            , style "width" "600px"
            , style "height" "200px"
            ]
            []
        , monodiv <|
            case Lisa.processString input of
                Ok result ->
                    result |> Lisa.Process.encodeProgram |> E.encode 2

                Err err ->
                    err.msg

        -- , monodiv <| E.encode 2 <| Lisa.Parser.parseToJson input
        ]


main : Program () String String
main =
    Browser.sandbox
        { init = ""
        , view = view
        , update = always
        }
