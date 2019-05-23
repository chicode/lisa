module Lisa.Process exposing
    ( Context
    , Expr(..)
    , ExprNode
    , MacroHandler
    , Program
    , encodeProgram
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


type alias Program =
    { vars : Dict String ( VarDecl, ExprNode )
    , funcs : Dict String FuncDecl
    }


type alias MacroHandler =
    (AstNode -> Result Error ExprNode)
    -> Location
    -> List AstNode
    -> Result Error ExprNode


type alias Context =
    { macros : Dict String MacroHandler
    }


emptyProgram : Program
emptyProgram =
    { vars = Dict.empty
    , funcs = Dict.empty
    }


processProgram : Context -> List AstNode -> Result Error Program
processProgram ctx ast =
    ast |> foldlListResult (processTopLevel ctx) emptyProgram


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
    case name.node of
        "if" ->
            processIf ctx loc args

        "set" ->
            processVar ctx loc "set" args
                |> Result.andThen
                    (\( var, expr ) -> Ok <| LocatedNode loc <| SetVar var expr)

        "list" ->
            args
                |> mapListResult (processExpr ctx)
                |> Result.map (LocatedNode loc << ListLit)

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
    -> Result Error ( SymbolNode, ExprNode )
processVar ctx loc name args =
    case args of
        var :: val :: [] ->
            case var.node of
                Symbol sym ->
                    processExpr ctx val
                        |> Result.map
                            (\expr -> ( mapNode var sym, expr ))

                _ ->
                    Err <|
                        nonRecovErrNode var <|
                            "First operand to '"
                                ++ name
                                ++ "' must be a symbol"

        var :: val :: rest ->
            Err <| nonRecovError loc <| "Too many operands to '" ++ name ++ "'"

        var :: [] ->
            Err <| nonRecovError loc "Missing what value to set the variable to"

        [] ->
            Err <| nonRecovError loc <| "Missing operands to '" ++ name ++ "'"


topLevelError : String
topLevelError =
    "Top level expression must be an var, const, or function declaration"


processTopLevel : Context -> AstNode -> Program -> Result Error Program
processTopLevel ctx expr program =
    case expr.node of
        List children ->
            case children of
                [] ->
                    Err <| nonRecovErrNode expr "Top level list cannot be empty"

                func :: args ->
                    case func.node of
                        Symbol sym ->
                            processTopLevelList ctx expr.loc (mapNode expr sym) args program

                        _ ->
                            Err <| nonRecovErrNode func topLevelError

        _ ->
            Err <| nonRecovErrNode expr topLevelError


processDef : Context -> Location -> List AstNode -> Program -> Result Error Program
processDef ctx loc args program =
    let
        argsError =
            nonRecovError loc <|
                "Expected 2 or more operands to def: the function name, "
                    ++ "a list of parameters, and then the function body"
    in
    case args of
        [] ->
            Err <| argsError

        _ :: [] ->
            Err <| argsError

        nameNode :: paramsNode :: bodyNodes ->
            let
                mapper : String -> List String -> List ExprNode -> Program
                mapper name params body =
                    { program
                        | funcs = Dict.insert name { params = params, body = body } program.funcs
                    }
            in
            Result.map3
                mapper
                (processSymbol nameNode
                    |> Result.andThen
                        (\symbolNode ->
                            if Dict.member symbolNode.node program.funcs then
                                Err <|
                                    nonRecovError
                                        loc
                                        "Cannot redefine previously defined function"

                            else
                                Ok symbolNode.node
                        )
                )
                (paramsNode
                    |> processListLit "Expected a list for the "
                    |> Result.andThen (mapListResult (processSymbol >> Result.map .node))
                )
                (bodyNodes |> mapListResult (processExpr ctx))


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
    -> Program
    -> Result Error Program
processTopLevelList ctx loc name args program =
    let
        processHandler body update =
            case body of
                Nothing ->
                    args
                        |> mapListResult (processExpr ctx)
                        |> Result.map update

                Just _ ->
                    Err <|
                        nonRecovError loc <|
                            "Duplicate "
                                ++ name.node
                                ++ " declaration"

        processVarDecl varType =
            processVar ctx loc name.node args
                |> Result.map
                    (\( var, expr ) ->
                        { program | vars = Dict.insert var.node ( varType, expr ) program.vars }
                    )
    in
    case name.node of
        "def" ->
            processDef ctx loc args program

        "var" ->
            processVarDecl Var

        "const" ->
            processVarDecl Const

        _ ->
            Err <| nonRecovError loc topLevelError


encodeProgram : Program -> E.Value
encodeProgram program =
    E.object
        [ ( "vars"
          , E.dict identity
                (\( declType, init ) ->
                    E.object
                        [ ( "type", encodeVarDecl declType )
                        , ( "init", encodeExpr init )
                        ]
                )
                program.vars
          )
        , ( "funcs", E.dict identity encodeFuncDecl program.funcs )
        ]


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
