module Lisa exposing (processString, processStringToJson)

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

    @docs processString
    @docs processStringToJson

-}

import Common exposing (Error)
import Json.Encode as E
import Lisa.Parser
import Lisa.Process exposing (Context)


{-| Parse a string into a program
-}
processString : String -> Context -> Result Error Lisa.Process.Program
processString input ctx =
    Lisa.Parser.parse input
        |> Result.andThen (Lisa.Process.processProgram ctx)


{-| Parse a string into a Json representation of a program
-}
processStringToJson : String -> Context -> Result E.Value E.Value
processStringToJson input ctx =
    processString input ctx
        |> Result.map Lisa.Process.encodeProgram
        |> Result.mapError Common.encodeError
