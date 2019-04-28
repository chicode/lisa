module Lisa exposing (lisaToJs, processString)

import Common exposing (Error)
import Lisa.Compile
import Lisa.Parser
import Lisa.Process


lisaToJs : String -> Result Error String
lisaToJs input =
    Ok ""


processString input =
    Lisa.Parser.parse input
        |> Result.andThen Lisa.Process.processProgram
