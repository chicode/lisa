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
    (defunc fib (n)
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
    (= (fib 10) 55)
    ```

    @docs parseProgram
    @docs parseProgramToJson
    @docs parseExpression
    @docs parseExpressionToJson

-}

import Common exposing (Error, encodeResult, mapListResult)
import Json.Encode as E
import Lisa.Parser
import Lisa.Process exposing (Context, ExprNode, encodeExpr)


{-| Parse a string into a program
-}
parseProgram : String -> Context -> Result Error (List ExprNode)
parseProgram input ctx =
    Lisa.Parser.parse input
        |> Result.andThen (Lisa.Process.processProgram ctx)


{-| Parse a string into a Json representation of a program
-}
parseProgramToJson : String -> Context -> E.Value
parseProgramToJson input ctx =
    parseProgram input ctx
        |> encodeResult (E.list encodeExpr)


{-| Parse a string into an expression
-}
parseExpression : String -> Context -> Result Error (List ExprNode)
parseExpression input ctx =
    Lisa.Parser.parse input
        |> Result.andThen (mapListResult (Lisa.Process.processExpr ctx))


{-| Parse a string into a Json representation of an expression
-}
parseExpressionToJson : String -> Context -> E.Value
parseExpressionToJson input ctx =
    parseExpression input ctx
        |> encodeResult (E.list encodeExpr)
