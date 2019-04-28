module Lisa.Process exposing (Expr(..), ExprNode, Program, processProgram)

import Common
    exposing
        ( Error
        , LocatedNode
        , Location
        , errNode
        , foldlListResult
        , mapListResult
        , mapNode
        )
import Lisa.Keys exposing (keyNameToCode)
import Lisa.Parser exposing (AstNode, SExpr(..))


type Expr
    = SetVar SymbolNode ExprNode
    | GetVar String
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
    { vars : List ( SymbolNode, ExprNode )
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
    case expr.node of
        Str s ->
            Ok <| mapNode expr <| StrLit s

        Num n ->
            Ok <| mapNode expr <| NumLit n

        Key keyname ->
            case keyNameToCode keyname of
                Just code ->
                    Ok <| mapNode expr <| KeyLit code

                Nothing ->
                    Err <|
                        errNode expr <|
                            "You have an invalid key name: '"
                                ++ keyname
                                ++ "'. Did you mean: "
                                ++ (Lisa.Keys.getClosestKeys keyname
                                        |> List.map (\k -> "'" ++ k ++ "'")
                                        |> List.intersperse " or "
                                        |> List.foldr (++) ""
                                   )
                                ++ "?"

        Symbol sym ->
            Ok <| mapNode expr <| GetVar sym

        List list ->
            case list of
                [] ->
                    Err <| errNode expr <| ""

                name :: args ->
                    case name.node of
                        Symbol sym ->
                            processList expr.loc (mapNode name sym) args

                        _ ->
                            Err <|
                                errNode name <|
                                    "First argument to a list must be a symbol"


processList : Location -> SymbolNode -> List AstNode -> Result Error ExprNode
processList loc name args =
    case name.node of
        "if" ->
            case args of
                cond :: body ->
                    Result.map2 (\c b -> LocatedNode loc <| If { cond = c, body = b })
                        (processExpr cond)
                        (mapListResult processExpr body)

                [] ->
                    Err <| errNode name <| "If missing condition"

        "set" ->
            processVar loc "set" args
                |> Result.map (\( var, expr ) -> LocatedNode loc <| SetVar var expr)

        _ ->
            mapListResult processExpr args
                |> Result.map (\body -> LocatedNode loc <| FuncCall name body)


processVar : Location -> String -> List AstNode -> Result Error ( SymbolNode, ExprNode )
processVar loc name args =
    case args of
        var :: val :: [] ->
            case var.node of
                Symbol sym ->
                    processExpr val
                        |> Result.map
                            (\expr -> ( mapNode var sym, expr ))

                _ ->
                    Err <|
                        errNode var <|
                            "First operand to '"
                                ++ name
                                ++ "' must be a symbol"

        var :: val :: rest ->
            Err <| Error loc <| "Too many operands to '" ++ name ++ "'"

        var :: [] ->
            Err <| Error loc "Missing what value to set the variable to"

        [] ->
            Err <| Error loc <| "Missing operands to '" ++ name ++ "'"


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
                            processTopLevelList expr.loc (mapNode expr sym) args program

                        _ ->
                            Err <| errNode func topLevelError

        _ ->
            Err <| errNode expr topLevelError


processTopLevelList :
    Location
    -> SymbolNode
    -> List AstNode
    -> Program
    -> Result Error Program
processTopLevelList loc name args program =
    let
        processHandler body update =
            case body of
                Nothing ->
                    args
                        |> mapListResult processExpr
                        |> Result.map update

                Just _ ->
                    Err <|
                        Error loc <|
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

        "var" ->
            processVar loc "var" args
                |> Result.map (\var -> { program | vars = var :: program.vars })

        _ ->
            Err <| Error loc topLevelError
