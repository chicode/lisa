module TestParser exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import LispParser exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "parser parses correctly"
        [ test "parses empty string" <|
            \_ -> parse "" |> Expect.equal (Ok [])
        , describe "parses lists"
            [ test "empty list" <|
                \_ -> parse "()" |> Expect.equal (Ok [ List [] ])
            ]
        ]
