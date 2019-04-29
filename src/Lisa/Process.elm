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
import Dict exposing (Dict)
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


type VarDecl
    = Var
    | Const


type alias Program =
    { vars : Dict String ( VarDecl, ExprNode )
    , init : Maybe (List ExprNode)
    , update : Maybe (List ExprNode)
    , draw : Maybe (List ExprNode)
    }


emptyProgram : Program
emptyProgram =
    { vars = Dict.empty
    , init = Nothing
    , update = Nothing
    , draw = Nothing
    }


processProgram : List AstNode -> Result Error Program
processProgram ast =
    ast |> foldlListResult processTopLevel emptyProgram


noVarError : String -> String
noVarError sym =
    "Variable '"
        ++ sym
        ++ "' has not been declared. To access a variable you must first define "
        ++ "it at the top level with (var varname initialvalue)"


processExpr : Program -> AstNode -> Result Error ExprNode
processExpr program expr =
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
            if Dict.member sym program.vars then
                Ok <| mapNode expr <| GetVar sym

            else
                Err <| errNode expr <| noVarError sym

        List list ->
            case list of
                [] ->
                    Err <| errNode expr <| ""

                name :: args ->
                    case name.node of
                        Symbol sym ->
                            processList program expr.loc (mapNode name sym) args

                        _ ->
                            Err <|
                                errNode name <|
                                    "First argument to a list must be a symbol"


processList : Program -> Location -> SymbolNode -> List AstNode -> Result Error ExprNode
processList program loc name args =
    case name.node of
        "if" ->
            case args of
                cond :: body ->
                    Result.map2 (\c b -> LocatedNode loc <| If { cond = c, body = b })
                        (processExpr program cond)
                        (mapListResult (processExpr program) body)

                [] ->
                    Err <| errNode name <| "If missing condition"

        "set" ->
            processVar program loc "set" args
                |> Result.andThen
                    (\( var, expr ) ->
                        case Dict.get var.node program.vars of
                            Just ( varType, _ ) ->
                                case varType of
                                    Var ->
                                        Ok <| LocatedNode loc <| SetVar var expr

                                    Const ->
                                        Err <|
                                            errNode var <|
                                                "You cannot change the value of a const. "
                                                    ++ "Maybe try changing the (var "
                                                    ++ var.node
                                                    ++ ") declaration to a (const) declaration."

                            Nothing ->
                                Err <|
                                    errNode var <|
                                        "You have to declare a variable at the top level "
                                            ++ "with (var varname initialvalue) "
                                            ++ "before you can set its value."
                    )

        _ ->
            mapListResult (processExpr program) args
                |> Result.map (\body -> LocatedNode loc <| FuncCall name body)


processVar :
    Program
    -> Location
    -> String
    -> List AstNode
    -> Result Error ( SymbolNode, ExprNode )
processVar program loc name args =
    case args of
        var :: val :: [] ->
            case var.node of
                Symbol sym ->
                    processExpr program val
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
                        |> mapListResult (processExpr program)
                        |> Result.map update

                Just _ ->
                    Err <|
                        Error loc <|
                            "Duplicate "
                                ++ name.node
                                ++ " declaration"

        processVarDecl varType =
            processVar program loc name.node args
                |> Result.map
                    (\( var, expr ) ->
                        { program | vars = Dict.insert var.node ( varType, expr ) program.vars }
                    )
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
            processVarDecl Var

        "const" ->
            processVarDecl Const

        _ ->
            Err <| Error loc topLevelError
