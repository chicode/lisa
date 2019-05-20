module Lisa exposing (processString, processStringToJson)

import Common exposing (Error)
import Json.Encode as E
import Lisa.Parser
import Lisa.Process


processString : String -> Result Error Lisa.Process.Program
processString input =
    Lisa.Parser.parse input
        |> Result.andThen Lisa.Process.processProgram


processStringToJson : String -> Result E.Value E.Value
processStringToJson input =
    processString input
        |> Result.map Lisa.Process.encodeProgram
        |> Result.mapError Common.encodeError
