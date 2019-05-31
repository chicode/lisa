module Main exposing (main)

import Browser
import Dict
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
        , input
            |> Lisa.parseProgramToJson { macros = Dict.empty }
            |> E.encode 2
            |> monodiv
        ]


main : Program () String String
main =
    Browser.sandbox
        { init = ""
        , view = view
        , update = always
        }
