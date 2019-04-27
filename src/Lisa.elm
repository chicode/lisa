module Lisa exposing (lisaToJs)

import Lisa.Compile
import Lisa.Parser
import Lisa.Process


type Error
    = Syntax Lisa.Parser.Error


lisaToJs : String -> Result Error String
lisaToJs input =
    Ok ""
