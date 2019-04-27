module Lisa.Process exposing (Expr(..), ExprNode, Program)

import Common
    exposing
        ( Error
        , LocatedNode
        , errNode
        , foldlListResult
        , mapListResult
        , mapNode
        )
import Lisa.Parser exposing (AstNode, SExpr(..))


type Expr
    = SetVar SymbolNode
    | GetVar SymbolNode
    | FuncCall SymbolNode (List ExprNode)
    | If { cond : ExprNode, body : List ExprNode }
    | StrLit String
    | NumLit Float
    | KeyLit Int


type alias ExprNode =
    LocatedNode Expr


type alias SymbolNode =
    LocatedNode String


type alias Program =
    { vars : List String
    , init : Maybe (List ExprNode)
    , update : Maybe (List ExprNode)
    , draw : Maybe (List ExprNode)
    }


emptyProgram : Program
emptyProgram =
    { vars = [], init = Nothing, update = Nothing, draw = Nothing }


processProgram : List AstNode -> Result Error Program
processProgram ast =
    ast |> foldlListResult processTopLevel emptyProgram


processExpr : AstNode -> Result Error ExprNode
processExpr expr =
    Ok <| mapNode (StrLit "") expr


topLevelError : String
topLevelError =
    "Top level expression must be an var, init, update, or draw declaration"


processTopLevel : AstNode -> Program -> Result Error Program
processTopLevel expr program =
    case expr.node of
        List children ->
            case children of
                [] ->
                    Err <| errNode expr "Top level list cannot be empty"

                func :: args ->
                    case func.node of
                        Symbol sym ->
                            processTopLevelCall (mapNode sym expr) args program

                        _ ->
                            Err <| errNode func topLevelError

        _ ->
            Err <| errNode expr topLevelError


processTopLevelCall : SymbolNode -> List AstNode -> Program -> Result Error Program
processTopLevelCall name args program =
    let
        processHandler body update =
            case body of
                Nothing ->
                    args
                        |> mapListResult processExpr
                        |> Result.map update

                Just _ ->
                    Err <|
                        errNode name <|
                            "Duplicate "
                                ++ name.node
                                ++ " declaration"
    in
    case name.node of
        "init" ->
            processHandler program.init <|
                \exprs -> { program | init = Just exprs }

        "update" ->
            processHandler program.update <|
                \exprs -> { program | update = Just exprs }

        "draw" ->
            processHandler program.draw <|
                \exprs -> { program | draw = Just exprs }

        _ ->
            Err <| errNode name topLevelError
