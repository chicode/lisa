module Lisa exposing
    ( parseExpression
    , parseExpressionToJson
    , parseProgram
    , parseProgramToJson
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

-}

import Json.Encode as E
import Lisa.Common exposing (Error, encodeResult, mapListResult)
import Lisa.Parser
import Lisa.Process exposing (ExprNode, encodeExpr)


{-| Parse a string into a program
-}
parseProgram : String -> Lisa.Process.Options -> Result Error (List ExprNode)
parseProgram input opts =
    Lisa.Parser.parse input
        |> Result.andThen (Lisa.Process.processProgram opts)


{-| Parse a string into a Json representation of a program
-}
parseProgramToJson : String -> Lisa.Process.Options -> E.Value
parseProgramToJson input opts =
    parseProgram input opts
        |> encodeResult (E.list encodeExpr)


{-| Parse a string into an expression
-}
parseExpression : String -> Lisa.Process.Options -> Result Error (List ExprNode)
parseExpression input opts =
    Lisa.Parser.parse input
        |> Result.andThen (mapListResult (Lisa.Process.processExprOpts opts))


{-| Parse a string into a Json representation of an expression
-}
parseExpressionToJson : String -> Lisa.Process.Options -> E.Value
parseExpressionToJson input opts =
    parseExpression input opts
        |> encodeResult (E.list encodeExpr)
