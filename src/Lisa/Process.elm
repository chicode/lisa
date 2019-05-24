module Lisa.Process exposing
    ( Context
    , Expr(..)
    , ExprNode
    , MacroHandler
    , encodeExpr
    , processExpr
    , processProgram
    )

import Common
    exposing
        ( Error
        , LocatedNode
        , Location
        , encodeWithLocation
        , foldlListResult
        , mapListResult
        , mapNode
        , nonRecovErrNode
        , nonRecovError
        )
import Dict exposing (Dict)
import Json.Encode as E
import Lisa.Parser exposing (AstNode, SExpr(..))
import Tuple


type Expr
    = SetVar SymbolNode ExprNode
    | GetVar String
    | FuncCall SymbolNode (List ExprNode)
    | If IfExpr
    | Do (List ExprNode)
    | Func FuncDecl
    | DefVar VarDecl SymbolNode (Maybe ExprNode)
    | DefFunc SymbolNode FuncDecl
    | StrLit String
    | NumLit Float
    | KeyLit Int
    | ListLit (List ExprNode)


type alias IfExpr =
    { cond : ExprNode
    , body : ExprNode
    , final : Maybe ExprNode
    }


type alias ExprNode =
    LocatedNode Expr


type alias SymbolNode =
    LocatedNode String


type VarDecl
    = Var
    | Const


type alias FuncDecl =
    { params : List String
    , body : List ExprNode
    }


type alias MacroHandler =
    (AstNode -> Result Error ExprNode)
    -> Location
    -> List AstNode
    -> Result Error ExprNode


type alias Context =
    { macros : Dict String MacroHandler
    }


processProgram : Context -> List AstNode -> Result Error (List ExprNode)
processProgram ctx =
    mapListResult (processTopLevel ctx)


processExpr : Context -> AstNode -> Result Error ExprNode
processExpr ctx expr =
    case expr.node of
        Str s ->
            Ok <| mapNode expr <| StrLit s

        Num n ->
            Ok <| mapNode expr <| NumLit n

        Symbol sym ->
            Ok <| mapNode expr <| GetVar sym

        List list ->
            case list of
                [] ->
                    Err <| nonRecovErrNode expr <| ""

                name :: args ->
                    case name.node of
                        Symbol sym ->
                            processList ctx expr.loc (mapNode name sym) args

                        _ ->
                            Err <|
                                nonRecovErrNode name <|
                                    "First argument to a list must be a symbol"


processList : Context -> Location -> SymbolNode -> List AstNode -> Result Error ExprNode
processList ctx loc name args =
    let
        processVarDecl varType =
            processVar ctx loc name.node args
                |> Result.map (\( var, init ) -> LocatedNode loc <| DefVar varType var init)
    in
    case name.node of
        "defunc" ->
            processDefunc ctx loc args

        "var" ->
            processVarDecl Var

        "const" ->
            processVarDecl Const

        "if" ->
            processIf ctx loc args

        "set" ->
            processVar ctx loc "set" args
                |> Result.andThen
                    (\( var, maybeExpr ) ->
                        case maybeExpr of
                            Just expr ->
                                Ok <| LocatedNode loc <| SetVar var expr

                            Nothing ->
                                Err <|
                                    nonRecovError loc
                                        "Missing what value to set the variable to"
                    )

        "list" ->
            args
                |> mapListResult (processExpr ctx)
                |> Result.map (LocatedNode loc << ListLit)

        "do" ->
            args
                |> mapListResult (processExpr ctx)
                |> Result.map (LocatedNode loc << Do)

        "func" ->
            processFunc ctx loc args |> Result.map (LocatedNode loc << Func)

        _ ->
            case Dict.get name.node ctx.macros of
                Just macro ->
                    macro (processExpr ctx) loc args

                Nothing ->
                    args
                        |> mapListResult (processExpr ctx)
                        |> Result.map (LocatedNode loc << FuncCall name)


processIf : Context -> Location -> List AstNode -> Result Error ExprNode
processIf ctx loc args =
    let
        err =
            nonRecovError loc <|
                "Expected 2 or 3 operands to 'if', got "
                    ++ (List.length args |> String.fromInt)
    in
    case args of
        condNode :: bodyNode :: rest ->
            Result.map3
                (\cond body final ->
                    LocatedNode loc <| If <| IfExpr cond body final
                )
                (processExpr ctx condNode)
                (processExpr ctx bodyNode)
                (case rest of
                    [] ->
                        Ok Nothing

                    finalNode :: [] ->
                        processExpr ctx finalNode |> Result.map Just

                    _ ->
                        Err err
                )

        _ ->
            Err err


processListLit : String -> AstNode -> Result Error (List AstNode)
processListLit msg node =
    case node.node of
        List list ->
            Ok list

        _ ->
            Err <| nonRecovErrNode node msg


processVar :
    Context
    -> Location
    -> String
    -> List AstNode
    -> Result Error ( SymbolNode, Maybe ExprNode )
processVar ctx loc name args =
    case args of
        var :: rest ->
            case var.node of
                Symbol sym ->
                    case rest of
                        [] ->
                            Ok ( mapNode var sym, Nothing )

                        val :: extra ->
                            if List.isEmpty extra then
                                processExpr ctx val
                                    |> Result.map
                                        (\expr -> ( mapNode var sym, Just expr ))

                            else
                                Err <|
                                    nonRecovError loc <|
                                        "Too many operands to '"
                                            ++ name
                                            ++ "'"

                _ ->
                    Err <|
                        nonRecovErrNode var <|
                            "First operand to '"
                                ++ name
                                ++ "' must be a symbol"

        [] ->
            Err <| nonRecovError loc <| "Missing operands to '" ++ name ++ "'"


topLevelError : String
topLevelError =
    "Top level expression must be an var, const, or function declaration"


processTopLevel : Context -> AstNode -> Result Error ExprNode
processTopLevel ctx expr =
    case expr.node of
        List children ->
            case children of
                [] ->
                    Err <| nonRecovErrNode expr "Top level list cannot be empty"

                func :: args ->
                    case func.node of
                        Symbol sym ->
                            processTopLevelList ctx expr.loc (mapNode expr sym) args

                        _ ->
                            Err <| nonRecovErrNode func topLevelError

        _ ->
            Err <| nonRecovErrNode expr topLevelError


processFunc : Context -> Location -> List AstNode -> Result Error FuncDecl
processFunc ctx loc args =
    case args of
        [] ->
            Err <| nonRecovError loc "Missing the argument list to function"

        paramsNode :: bodyNodes ->
            Result.map2 FuncDecl
                (paramsNode
                    |> processListLit "Expected a list for the parameters list"
                    |> Result.andThen (mapListResult (processSymbol >> Result.map .node))
                )
                (bodyNodes |> mapListResult (processExpr ctx))


processDefunc : Context -> Location -> List AstNode -> Result Error ExprNode
processDefunc ctx loc args =
    case args of
        [] ->
            Err <|
                nonRecovError loc <|
                    "Expected 2 or more operands to defunc: the function name, "
                        ++ "a list of parameters, and then the function body"

        nameNode :: rest ->
            Result.map2 (\name func -> LocatedNode loc <| DefFunc name func)
                (processSymbol nameNode)
                (processFunc ctx loc rest)


processSymbol : AstNode -> Result Error SymbolNode
processSymbol node =
    case node.node of
        Symbol s ->
            Ok <| mapNode node s

        _ ->
            Err <| nonRecovErrNode node "Expected a symbol"


processTopLevelList :
    Context
    -> Location
    -> SymbolNode
    -> List AstNode
    -> Result Error ExprNode
processTopLevelList ctx loc name args =
    let
        processVarDeclNoInit varType =
            processVar ctx loc name.node args
                |> Result.andThen
                    (\( var, init ) ->
                        case init of
                            Nothing ->
                                Ok <| LocatedNode loc <| DefVar varType var Nothing

                            Just initExpr ->
                                Err <| nonRecovErrNode initExpr ""
                    )
    in
    case name.node of
        "defunc" ->
            processDefunc ctx loc args

        "var" ->
            processVarDeclNoInit Var

        "const" ->
            processVarDeclNoInit Const

        _ ->
            Err <| nonRecovError loc topLevelError


encodeVarDecl : VarDecl -> E.Value
encodeVarDecl varDecl =
    case varDecl of
        Var ->
            E.string "var"

        Const ->
            E.string "const"


encodeFuncDecl : FuncDecl -> E.Value
encodeFuncDecl { params, body } =
    E.object
        [ ( "params", E.list E.string params )
        , ( "body", E.list encodeExpr body )
        ]


encodeExpr : ExprNode -> E.Value
encodeExpr expr =
    encodeWithLocation expr.loc <|
        case expr.node of
            SetVar var val ->
                [ ( "type", E.string "setVar" )
                , ( "var", encodeSymbol var )
                , ( "val", encodeExpr val )
                ]

            GetVar var ->
                [ ( "type", E.string "getVar" )
                , ( "var", encodeSymbol <| mapNode expr var )
                ]

            FuncCall func args ->
                [ ( "type", E.string "funcCall" )
                , ( "func", encodeSymbol func )
                , ( "args", E.list encodeExpr args )
                ]

            If { cond, body, final } ->
                [ ( "type", E.string "if" )
                , ( "cond", encodeExpr cond )
                , ( "body", encodeExpr body )
                , ( "final", encodeMaybe encodeExpr final )
                ]

            Do body ->
                [ ( "type", E.string "do" )
                , ( "body", E.list encodeExpr body )
                ]

            Func func ->
                [ ( "type", E.string "func" )
                , ( "func", encodeFuncDecl func )
                ]

            DefVar varType var init ->
                [ ( "type", E.string "defVar" )
                , ( "varType", encodeVarDecl varType )
                , ( "var", encodeSymbol var )
                , ( "init", encodeMaybe encodeExpr init )
                ]

            DefFunc name func ->
                [ ( "type", E.string "defFunc" )
                , ( "name", encodeSymbol name )
                , ( "func", encodeFuncDecl func )
                ]

            StrLit s ->
                [ ( "type", E.string "strLit" )
                , ( "value", E.string s )
                ]

            NumLit n ->
                [ ( "type", E.string "numLit" )
                , ( "value", E.float n )
                ]

            KeyLit k ->
                [ ( "type", E.string "numLit" )
                , ( "value", E.int k )
                ]

            ListLit list ->
                [ ( "type", E.string "list" )
                , ( "elements", E.list encodeExpr list )
                ]


encodeMaybe : (a -> E.Value) -> Maybe a -> E.Value
encodeMaybe func maybe =
    case maybe of
        Just a ->
            func a

        Nothing ->
            E.null


encodeSymbol : SymbolNode -> E.Value
encodeSymbol sym =
    encodeWithLocation sym.loc
        [ ( "name", E.string sym.node )
        ]
