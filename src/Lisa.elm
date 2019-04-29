module Lisa exposing (compileString, processString)

import Common exposing (Error)
import Lisa.Compile
import Lisa.Parser
import Lisa.Process


processString : String -> Result Error Lisa.Process.Program
processString input =
    Lisa.Parser.parse input
        |> Result.andThen Lisa.Process.processProgram


compileString : String -> Result Error String
compileString input =
    processString input |> Result.map Lisa.Compile.compile
