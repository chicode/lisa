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
    (def fib (n)
        (if (<= n 2) 1
            (-
                (fib (- n 1))
                (fib (- n 2)))))
    (var fib10 (fib 10))
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
