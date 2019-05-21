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
import Lisa.Process


{-| Parse a string into a program
-}
processString : String -> Result Error Lisa.Process.Program
processString input =
    Lisa.Parser.parse input
        |> Result.andThen Lisa.Process.processProgram


{-| Parse a string into a Json representation of a program
-}
processStringToJson : String -> Result E.Value E.Value
processStringToJson input =
    processString input
        |> Result.map Lisa.Process.encodeProgram
        |> Result.mapError Common.encodeError
