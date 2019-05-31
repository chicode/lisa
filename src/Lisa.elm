module Lisa exposing
    ( parseExpression
    , parseExpressionToJson
    , parseProgram
    , parseProgramToJson
    , parseReplExpression
    , parseReplExpressionToJson
    )

{-|

    # Lisa

    A simple lisp
    Example:
    ```
    (defunc fib-recurse (n)
        (var a 0) (var b 1) (var f 1)
        (defunc calc-fib (i)
            (if (<= i n)
                (do
                    (set f (+ a b))
                    (set a b)
                    (set b f)
                    (calc-fib (+ i 1)))
                f))
        (calc-fib 2))
    (defunc fib-while (n)
        (var a 0) (var b 1) (var f 1)
        (var i 2)
        (while (<= i n)
            (set f (+ a b))
            (set a b)
            (set b f)
            (set i (+ i 1))
            f))
    (= (fib-recurse 10) (fib-while 10) 55)
    ```

    @docs parseProgram
    @docs parseProgramToJson
    @docs parseExpression
    @docs parseExpressionToJson
    @docs parseReplExpression
    @docs parseReplExpressionToJson

-}

import Json.Encode as E
import Lisa.Common exposing (Error, encodeResult, mapListResult)
import Lisa.Parser
import Lisa.Process
    exposing
        ( ExprNode
        , Program
        , ReplExpression
        , encodeExpr
        , encodeReplExpression
        )


{-| Parse a string into a program
-}
parseProgram : Lisa.Process.Options -> String -> Result Error Program
parseProgram opts input =
    Lisa.Parser.parse input
        |> Result.andThen (Lisa.Process.processProgram opts)


{-| Parse a string into a Json representation of a program
-}
parseProgramToJson : Lisa.Process.Options -> String -> E.Value
parseProgramToJson opts input =
    input
        |> parseProgram opts
        |> encodeResult (E.dict identity encodeExpr)


{-| Parse a string into an expression
-}
parseExpression : Lisa.Process.Options -> String -> Result Error (List ExprNode)
parseExpression opts input =
    Lisa.Parser.parse input
        |> Result.andThen (mapListResult (Lisa.Process.processExprOpts opts))


{-| Parse a string into a Json representation of an expression
-}
parseExpressionToJson : Lisa.Process.Options -> String -> E.Value
parseExpressionToJson opts input =
    input
        |> parseExpression opts
        |> encodeResult (E.list encodeExpr)


{-| Parse a string into an expression intended for a REPL
-}
parseReplExpression : Lisa.Process.Options -> String -> Result Error (List ReplExpression)
parseReplExpression opts input =
    Lisa.Parser.parse input
        |> Result.andThen (mapListResult (Lisa.Process.processReplExpr opts))


{-| Parse a string into a Json representation of an expression intended for a REPL
-}
parseReplExpressionToJson : Lisa.Process.Options -> String -> E.Value
parseReplExpressionToJson opts input =
    input
        |> parseReplExpression opts
        |> encodeResult (E.list encodeReplExpression)
