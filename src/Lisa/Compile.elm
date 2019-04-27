module Lisa.Compile exposing (compile)

import Common exposing (LocatedNode, foldlListResult, mapNode)
import Json.Encode as E
import Lisa.Process exposing (Expr(..), ExprNode, Program)


compile : Program -> String
compile =
    compileProgram


boilerplate : String
boilerplate =
    """
var funcs = Object.create(null)
Object.assign(funcs, mars)
"""


compileProgram : Program -> String
compileProgram program =
    let
        compileTopLevel name maybeBody =
            let
                compiled =
                    case maybeBody of
                        Just body ->
                            List.map compileExpr body |> String.join "\n"

                        Nothing ->
                            ""
            in
            "function "
                ++ name
                ++ "(){"
                ++ compiled
                ++ "}"
    in
    boilerplate
        ++ compileTopLevel "init" program.init
        ++ compileTopLevel "update" program.update
        ++ compileTopLevel "draw" program.draw


compileExpr : ExprNode -> String
compileExpr expr =
    case expr.node of
        StrLit s ->
            E.encode 0 <| E.string s

        NumLit n ->
            E.encode 0 <| E.float n

        KeyLit k ->
            E.encode 0 <| E.int k

        _ ->
            Debug.todo "other expressions"
