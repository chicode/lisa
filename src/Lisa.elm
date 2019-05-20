module Lisa exposing (compileString, processString)

import Common exposing (Error)
import Lisa.Parser
import Lisa.Process


processString : String -> Result Error Lisa.Process.Program
processString input =
    Lisa.Parser.parse input
        |> Result.andThen Lisa.Process.processProgram
