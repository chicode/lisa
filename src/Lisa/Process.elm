module Lisa.Process exposing (Expr(..), ExprNode, Program, encodeProgram, processProgram)

import Common
    exposing
        ( Error
        , LocatedNode
        , Location
        , encodeWithLocation
        , errNode
        , foldlListResult
        , mapListResult
        , mapNode
        )
import Dict exposing (Dict)
import Json.Encode as E
import Lisa.Keys exposing (keyNameToCode)
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


emptyProgram : Program
emptyProgram =
    { vars = Dict.empty
    , funcs = Dict.empty
    }


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
            processIf loc args

        "set" ->
            processVar loc "set" args
                |> Result.andThen
                    (\( var, expr ) -> Ok <| LocatedNode loc <| SetVar var expr)

        _ ->
            mapListResult processExpr args
                |> Result.map (LocatedNode loc << FuncCall name)


processIf : Location -> List AstNode -> Result Error ExprNode
processIf loc args =
    let
        err =
            Error loc <|
                "Expected 2 or 3 operands to 'if', got "
                    ++ (List.length args |> String.fromInt)
    in
    case args of
        condNode :: bodyNode :: rest ->
            Result.map3
                (\cond body final ->
                    LocatedNode loc <| If <| IfExpr cond body final
                )
                (processExpr condNode)
                (processExpr bodyNode)
                (case rest of
                    [] ->
                        Ok Nothing

                    finalNode :: [] ->
                        processExpr finalNode |> Result.map Just

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
            Err <| errNode node msg


processVar :
    Location
    -> String
    -> List AstNode
    -> Result Error ( SymbolNode, ExprNode )
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
    "Top level expression must be an var, const, or function declaration"


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


processDef : Location -> List AstNode -> Program -> Result Error Program
processDef loc args program =
    let
        argsError =
            Error loc <|
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
                                    Error loc "Cannot redefine previously defined function"

                            else
                                Ok symbolNode.node
                        )
                )
                (paramsNode
                    |> processListLit "Expected a list for the "
                    |> Result.andThen (mapListResult (processSymbol >> Result.map .node))
                )
                (bodyNodes |> mapListResult processExpr)


processSymbol : AstNode -> Result Error SymbolNode
processSymbol node =
    case node.node of
        Symbol s ->
            Ok <| mapNode node s

        _ ->
            Err <| errNode node "Expected a symbol"


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

        processVarDecl varType =
            processVar loc name.node args
                |> Result.map
                    (\( var, expr ) ->
                        { program | vars = Dict.insert var.node ( varType, expr ) program.vars }
                    )
    in
    case name.node of
        "def" ->
            processDef loc args program

        "var" ->
            processVarDecl Var

        "const" ->
            processVarDecl Const

        _ ->
            Err <| Error loc topLevelError


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
