module Lisa.Compile exposing (compile)

import Common exposing (LocatedNode, foldlListResult, mapNode)
import Dict
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
var vars = Object.create(null)
"""


toJsonString : String -> String
toJsonString s =
    E.encode 0 <| E.string s


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
            "\nfunction "
                ++ name
                ++ "(){"
                ++ compiled
                ++ "}"

        assignVars vars =
            Dict.toList vars
                |> List.map
                    (\( var, ( _, expr ) ) ->
                        "vars["
                            ++ toJsonString var
                            ++ "]="
                            ++ compileExpr expr
                    )
                |> String.join "\n"
    in
    boilerplate
        ++ assignVars program.vars
        ++ compileTopLevel "init" program.init
        ++ compileTopLevel "update" program.update
        ++ compileTopLevel "draw" program.draw


compileExpr : ExprNode -> String
compileExpr expr =
    case expr.node of
        StrLit s ->
            toJsonString s

        NumLit n ->
            E.encode 0 <| E.float n

        KeyLit k ->
            E.encode 0 <| E.int k

        SetVar var val ->
            "vars[" ++ toJsonString var.node ++ "]=" ++ compileExpr val

        GetVar var ->
            "vars[" ++ toJsonString var ++ "]"

        FuncCall func args ->
            "funcs["
                ++ toJsonString func.node
                ++ "]("
                ++ (List.map compileExpr args |> String.join ",")
                ++ ")"

        If { cond, body } ->
            "if("
                ++ compileExpr cond
                ++ "){"
                ++ (List.map compileExpr body |> String.join "\n")
                ++ "}"
