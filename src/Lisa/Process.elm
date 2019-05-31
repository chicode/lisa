module Lisa.Process exposing
    ( Context
    , Expr(..)
    , ExprNode
    , MacroHandler
    , encodeExpr
    , processExpr
    , processExprOpts
    , processProgram
    , Options
    , Program
    , ReplExpression(..)
    , encodeReplExpression
    , processReplExpr
    )

{-|

@docs Context
@docs Expr
@docs ExprNode
@docs MacroHandler
@docs encodeExpr
@docs processExpr
@docs processExprOpts
@docs processProgram
@docs Options
@docs Program
@docs ReplExpression
@docs encodeReplExpression
@docs processReplExpr

-}

import Dict exposing (Dict)
import Json.Encode as E
import Lisa.Common
    exposing
        ( Error
        , LocatedNode
        , Location
        , encodeWithLocation
        , foldlListResult
        , groupListEvery2
        , mapListResult
        , mapNode
        , nonRecovErrNode
        , nonRecovError
        )
import Lisa.Parser exposing (AstNode, SExpr(..))
import List.Extra
import Set exposing (Set)
import Tuple


{-| -}
type Expr
    = GetSymbol String
    | FuncCall SymbolNode (List ExprNode)
    | Cond CondExpr
    | Func FuncDecl
    | Let (List ( String, ExprNode )) ExprNode
    | NoneLit
    | BoolLit Bool
    | NumLit Float
    | StrLit String
    | ListLit (List ExprNode)


type alias CondExpr =
    { clauses : List ( ExprNode, ExprNode )
    , otherwise : ExprNode
    }


{-| -}
type alias ExprNode =
    LocatedNode Expr


type alias SymbolNode =
    LocatedNode String


type VarDecl
    = Var
    | Const


type alias FuncDecl =
    { params : List String
    , body : ExprNode
    }


{-| -}
type alias MacroHandler =
    Context
    -> Location
    -> List AstNode
    -> Result Error ExprNode


{-| -}
type alias Options =
    { macros : Dict String MacroHandler
    }


{-| -}
type alias Program =
    Dict String ExprNode


{-|

    This is sort of strange in order to fix a problem with recursive type aliases.

-}
type Context
    = Context Options


getCtx : Context -> Options
getCtx (Context ctx) =
    ctx


{-| -}
processProgram : Options -> List AstNode -> Result Error Program
processProgram opts =
    let
        ctx =
            Context opts
    in
    foldlListResult
        (\expr program ->
            expr
                |> processTopLevel ctx
                |> Maybe.withDefault
                    (Err <|
                        nonRecovErrNode expr <|
                            "Top level of a program can only be a (defunc) or "
                                ++ "(defconst) declaration"
                    )
                |> Result.andThen
                    (\( name, val ) ->
                        if program |> Dict.member name.node then
                            Err <|
                                nonRecovErrNode name <|
                                    "You cannot redefine '"
                                        ++ name.node
                                        ++ "'"

                        else
                            program |> Dict.insert name.node val |> Ok
                    )
        )
        Dict.empty


{-| -}
type ReplExpression
    = Expression ExprNode
    | Definition String ExprNode


{-| -}
processReplExpr : Options -> AstNode -> Result Error ReplExpression
processReplExpr opts expr =
    let
        ctx =
            Context opts
    in
    case processTopLevel ctx expr of
        Just result ->
            result
                |> Result.map
                    (\( name, value ) -> Definition name.node value)

        Nothing ->
            processExpr ctx expr |> Result.map Expression


{-| -}
processExprOpts : Options -> AstNode -> Result Error ExprNode
processExprOpts opts =
    processExpr (Context opts)


{-| -}
processExpr : Context -> AstNode -> Result Error ExprNode
processExpr ctx expr =
    case expr.node of
        Str s ->
            Ok <| mapNode expr <| StrLit s

        Num n ->
            Ok <| mapNode expr <| NumLit n

        Symbol sym ->
            case sym of
                "none" ->
                    Ok <| mapNode expr NoneLit

                "true" ->
                    Ok <| mapNode expr <| BoolLit True

                "false" ->
                    Ok <| mapNode expr <| BoolLit False

                _ ->
                    sym
                        |> mapNode expr
                        |> validSymbol
                        |> Result.map (mapNode expr << GetSymbol << .node)

        List list ->
            list
                |> mapListResult (processExpr ctx)
                |> Result.map (mapNode expr << ListLit)

        Group group ->
            case group of
                [] ->
                    Err <| nonRecovErrNode expr <| ""

                name :: args ->
                    case name.node of
                        Symbol sym ->
                            processGroup ctx expr.loc (mapNode name sym) args

                        _ ->
                            Err <|
                                nonRecovErrNode name <|
                                    "First element in a group must be a symbol"


processGroup : Context -> Location -> SymbolNode -> List AstNode -> Result Error ExprNode
processGroup ctx loc name args =
    case name.node of
        "cond" ->
            processCond ctx loc args

        "func" ->
            processFunc ctx loc args

        "let" ->
            processLet ctx loc args

        _ ->
            case Dict.get name.node (getCtx ctx).macros of
                Just macro ->
                    macro ctx loc args

                Nothing ->
                    Result.map2 FuncCall
                        (validSymbol name)
                        (args |> mapListResult (processExpr ctx))
                        |> Result.map (LocatedNode loc)


processFunc : MacroHandler
processFunc ctx loc args =
    case args of
        [] ->
            Err <|
                nonRecovError loc
                    "Missing the argument list for defining this function"

        params :: body :: [] ->
            Result.map2 FuncDecl
                (params
                    |> processGrouping "Expected a list for the parameters list"
                    |> Result.andThen (mapListResult (processSymbol >> Result.map .node))
                )
                (processExpr ctx body)
                |> Result.map (LocatedNode loc << Func)

        _ ->
            Err <|
                nonRecovError loc <|
                    "Too many arguments to define this function; the body of a "
                        ++ "function should be one expression, if you need to "
                        ++ "do a lot of stuff in one function, try a (let ...) expression."


processLet : MacroHandler
processLet ctx loc args =
    let
        formErr =
            "let should be in the form of (let [symbolone exprone "
                ++ "symboltwo exprtwo symboln exprn] bodyexpr)"
    in
    case args of
        defsNode :: bodyNode :: [] ->
            Result.map2 Let
                (defsNode
                    |> processListGroup formErr
                    |> Result.andThen
                        (groupListEvery2
                            >> Result.mapError
                                (\odd -> nonRecovErrNode odd formErr)
                        )
                    |> Result.andThen
                        (mapListResult
                            (\( name, val ) ->
                                Result.map2 Tuple.pair
                                    (processSymbol name |> Result.map .node)
                                    (processExpr ctx val)
                            )
                        )
                )
                (processExpr ctx bodyNode)
                |> Result.map (LocatedNode loc)

        _ ->
            Err <| nonRecovError loc formErr


processCond : MacroHandler
processCond ctx loc args =
    let
        formErr =
            "cond should be in the form of (cond [conditionOne expressionOne] "
                ++ "[conditionTwo expressionTwo] [conditionN expressionN] "
                ++ "[else otherwise])"
    in
    case List.Extra.unconsLast args of
        Just ( otherwise, clauses ) ->
            Result.map2 CondExpr
                (mapListResult
                    (processTuple formErr (processExpr ctx) (processExpr ctx))
                    clauses
                )
                (otherwise
                    |> processTuple formErr
                        (\{ node } ->
                            if node == Symbol "else" then
                                Ok ()

                            else
                                Err <|
                                    nonRecovError loc
                                        "Missing [else ] condition in case expression"
                        )
                        (processExpr ctx)
                    |> Result.map Tuple.second
                )
                |> Result.map (LocatedNode loc << Cond)

        Nothing ->
            Err <| nonRecovError loc formErr


processTopLevel :
    Context
    -> AstNode
    -> Maybe (Result Error ( SymbolNode, ExprNode ))
processTopLevel ctx expr =
    let
        processTopLevelGroup sym args =
            case sym of
                "defunc" ->
                    Just <| processDefunc ctx expr.loc args

                "defconst" ->
                    Just <| processDefconst ctx expr.loc args

                _ ->
                    Nothing
    in
    case expr.node of
        Group children ->
            case children of
                func :: args ->
                    case func.node of
                        Symbol sym ->
                            processTopLevelGroup sym args

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


processDefunc : Context -> Location -> List AstNode -> Result Error ( SymbolNode, ExprNode )
processDefunc ctx loc args =
    case args of
        [] ->
            Err <|
                nonRecovError loc <|
                    "Expected 2 or more operands to defunc: the function name, "
                        ++ "a list of parameters, and then the function body"

        nameNode :: rest ->
            Result.map2 Tuple.pair
                (processSymbol nameNode)
                (processFunc ctx loc rest)


processDefconst :
    Context
    -> Location
    -> List AstNode
    -> Result Error ( SymbolNode, ExprNode )
processDefconst ctx loc args =
    case args of
        nameNode :: bodyNode :: [] ->
            Result.map2 Tuple.pair
                (processSymbol nameNode)
                (processExpr ctx bodyNode)

        _ ->
            Err <| nonRecovError loc "defconst must be of form (defconst constname expr)"


processGrouping : String -> AstNode -> Result Error (List AstNode)
processGrouping msg node =
    case node.node of
        Group group ->
            Ok group

        _ ->
            Err <| nonRecovErrNode node msg


processListGroup : String -> AstNode -> Result Error (List AstNode)
processListGroup msg node =
    case node.node of
        List list ->
            Ok list

        _ ->
            Err <| nonRecovErrNode node msg


processSymbol : AstNode -> Result Error SymbolNode
processSymbol node =
    case node.node of
        Symbol s ->
            s |> mapNode node |> validSymbol

        _ ->
            Err <| nonRecovErrNode node "Expected a symbol"


processTuple :
    String
    -> (AstNode -> Result Error a)
    -> (AstNode -> Result Error b)
    -> AstNode
    -> Result Error ( a, b )
processTuple errMsg mapFirst mapSecond node =
    node
        |> processListGroup errMsg
        |> Result.andThen
            (\list ->
                case list of
                    first :: second :: [] ->
                        Result.map2 Tuple.pair
                            (mapFirst first)
                            (mapSecond second)

                    _ ->
                        Err <| nonRecovErrNode node errMsg
            )


invalidIdents : Set String
invalidIdents =
    Set.fromList
        [ "defconst"
        , "defunc"
        , "if"
        , "let"
        , "func"
        , "cond"
        , "else"
        , "none"
        , "true"
        , "false"
        ]


isValidIdent : String -> Bool
isValidIdent sym =
    invalidIdents |> Set.member sym |> not


validSymbol : SymbolNode -> Result Error SymbolNode
validSymbol sym =
    if isValidIdent sym.node then
        Ok sym

    else
        Err <|
            nonRecovErrNode sym <|
                "Invalid identifier, try using something other than '"
                    ++ sym.node
                    ++ "'"


{-| -}
encodeExpr : ExprNode -> E.Value
encodeExpr expr =
    encodeWithLocation expr.loc <|
        case expr.node of
            GetSymbol sym ->
                [ ( "type", E.string "getSymbol" )
                , ( "symbol", encodeSymbol <| mapNode expr sym )
                ]

            FuncCall func args ->
                [ ( "type", E.string "funcCall" )
                , ( "func", encodeSymbol func )
                , ( "args", E.list encodeExpr args )
                ]

            Cond { clauses, otherwise } ->
                [ ( "type", E.string "if" )
                , ( "clauses"
                  , E.list
                        (\( cond, val ) ->
                            E.object
                                [ ( "cond", encodeExpr cond )
                                , ( "val", encodeExpr val )
                                ]
                        )
                        clauses
                  )
                , ( "otherwise", encodeExpr otherwise )
                ]

            Func { params, body } ->
                [ ( "type", E.string "func" )
                , ( "params", E.list E.string params )
                , ( "body", encodeExpr body )
                ]

            Let defs body ->
                [ ( "type", E.string "let" )
                , ( "defs"
                  , E.list
                        (\( name, val ) ->
                            E.object
                                [ ( "name", E.string name )
                                , ( "val", encodeExpr val )
                                ]
                        )
                        defs
                  )
                , ( "body", encodeExpr body )
                ]

            NoneLit ->
                [ ( "type", E.string "noneLit" ) ]

            BoolLit b ->
                [ ( "type", E.string "boolLit" )
                , ( "value", E.bool b )
                ]

            NumLit n ->
                [ ( "type", E.string "numLit" )
                , ( "value", E.float n )
                ]

            StrLit s ->
                [ ( "type", E.string "strLit" )
                , ( "value", E.string s )
                ]

            ListLit list ->
                [ ( "type", E.string "list" )
                , ( "elements", E.list encodeExpr list )
                ]


encodeSymbol : SymbolNode -> E.Value
encodeSymbol sym =
    encodeWithLocation sym.loc
        [ ( "name", E.string sym.node )
        ]


{-| -}
encodeReplExpression : ReplExpression -> E.Value
encodeReplExpression replExpression =
    case replExpression of
        Expression expr ->
            E.object
                [ ( "type", E.string "expression" )
                , ( "expr", encodeExpr expr )
                ]

        Definition name val ->
            E.object
                [ ( "type", E.string "definition" )
                , ( "name", E.string "name" )
                , ( "value", encodeExpr val )
                ]
