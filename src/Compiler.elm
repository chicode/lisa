module Compiler exposing (compile)

import LispParser exposing (ExprNode, SExpr(..))


type alias Error =
    { msg : String
    , startPos : ( Int, Int )
    , endPos : ( Int, Int )
    }


type alias Program =
    { init : Maybe (List ExprNode)
    , update : Maybe (List ExprNode)
    , draw : Maybe (List ExprNode)
    }


errNode : ExprNode -> String -> Error
errNode { startPos, endPos } msg =
    { msg = msg, startPos = startPos, endPos = endPos }


emptyProgram =
    { init = Nothing, update = Nothing, draw = Nothing }


compile : List ExprNode -> Result Error String
compile ast =
    ast
        |> processProgram
        |> Result.andThen compileProgram


compileProgram : Program -> Result Error String
compileProgram program =
    Ok ""


processProgram : List ExprNode -> Result Error Program
processProgram ast =
    List.foldl (\expr program -> Result.andThen (processTopLevel expr) program)
        (Ok emptyProgram)
        ast


processTopLevel : ExprNode -> Program -> Result Error Program
processTopLevel expr program =
    let
        mustBeDecl =
            "Top level expression must be an init, update, or draw declaration"
    in
    case expr.expr of
        List children ->
            case children of
                [] ->
                    Err <| errNode expr "Top level list cannot be empty"

                func :: args ->
                    case func.expr of
                        Symbol sym ->
                            case sym of
                                "init" ->
                                    case program.init of
                                        Nothing ->
                                            Ok { program | init = Just args }

                                        Just _ ->
                                            Err <| errNode expr "Duplicate init declaration"

                                "update" ->
                                    case program.update of
                                        Nothing ->
                                            Ok { program | update = Just args }

                                        Just _ ->
                                            Err <| errNode expr "Duplicate update declaration"

                                "draw" ->
                                    case program.draw of
                                        Nothing ->
                                            Ok { program | draw = Just args }

                                        Just _ ->
                                            Err <| errNode expr "Duplicate draw declaration"

                                _ ->
                                    Err <| errNode func mustBeDecl

                        _ ->
                            Err <| errNode func mustBeDecl

        _ ->
            Err <| errNode expr mustBeDecl
