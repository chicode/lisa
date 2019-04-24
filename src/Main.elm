module Main exposing (main)

import Browser
import Html exposing (Html, div, text, textarea)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onInput)
import LispParser


view : String -> Html String
view model =
    div []
        [ textarea
            [ onInput identity
            , style "width" "600px"
            , style "height" "200px"
            ]
            []
        , div
            [ style "font-family" "monospace"
            , style "white-space" "pre"
            ]
            [ text <| LispParser.parseToStringRepr model ]
        ]


main =
    Browser.sandbox
        { init = ""
        , view = view
        , update = always
        }
