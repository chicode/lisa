module LispParser exposing
    ( encodeSExpr
    , errorToString
    , parse
    , parseToJson
    )

import Json.Encode as E
import Keys exposing (keyNameToCode)
import Maybe
import Parser.Advanced as Parser exposing (..)
import Set


type Problem
    = ExpectedExpr
    | ExpectedKeyName
    | ExpectedListEnd
    | UnexpectedStringEnd
    | InvalidNumber
    | InvalidStringEscape
    | InvalidKey String
    | Never


type Context
    = StrLit
    | ListLit
    | KeyLit
    | TopCtx


type alias Parser a =
    Parser.Parser Context Problem a


type alias Error =
    DeadEnd Context Problem


type SExpr
    = List (List SExpr)
    | Symbol String
    | Str String
    | Num Float
    | Key Int


type alias ErrRepr =
    { pos : ( Int, Int )
    , startPos : ( Int, Int )
    , context : Context
    , msg : String
    }


reprErr : Error -> ErrRepr
reprErr err =
    let
        { row, col, contextStack } =
            err

        -- hacky, I don't know why this is necessary
        currentCtx =
            case contextStack of
                [] ->
                    Nothing

                top :: rest ->
                    Just <|
                        case top.context of
                            ListLit ->
                                List.head rest |> Maybe.withDefault top

                            _ ->
                                top

        ( startPos, context ) =
            case currentCtx of
                Just c ->
                    ( ( c.row, c.col ), c.context )

                Nothing ->
                    ( ( 1, 1 ), TopCtx )
    in
    { pos = ( row, col )
    , startPos = startPos
    , context = context
    , msg = errorToString err
    }


pickErr : List Error -> Maybe Error
pickErr errs =
    errs
        |> List.sortBy
            (\err ->
                case err.problem of
                    ExpectedKeyName ->
                        1

                    _ ->
                        10
            )
        |> List.head


errorToString : Error -> String
errorToString err =
    case err.problem of
        ExpectedExpr ->
            "Expected an expression, like a list: (a b c), string: \"abc\", or number: 123"

        ExpectedKeyName ->
            "Expected the name of a key. There cannot be any spaces between the '@' and the key name"

        ExpectedListEnd ->
            "Expected the end of a list, but couldn't find it. Try adding a ')'."

        UnexpectedStringEnd ->
            "Couldn't find a closing '\"' for string literal."

        InvalidNumber ->
            "You have a malformed number literal."

        InvalidStringEscape ->
            "You have an invalid character after a '\\' in your string. To "
                ++ "represent a raw '\\', you need to put 2 of them, like: '\\\\'"

        InvalidKey keyname ->
            "You have an invalid key name: '"
                ++ keyname
                ++ "'. Did you mean: "
                ++ (Keys.getClosestKeys keyname
                        |> List.map (\k -> "'" ++ k ++ "'")
                        |> List.intersperse " or "
                        |> List.foldr (++) ""
                   )
                ++ "?"

        Never ->
            "You should never see this error message"


encodeErrRepr : ErrRepr -> E.Value
encodeErrRepr { pos, startPos, context, msg } =
    let
        ( row, col ) =
            pos

        ( startRow, startCol ) =
            startPos
    in
    E.object
        [ ( "msg", E.string msg )
        , ( "col", E.int col )
        , ( "row", E.int row )
        , ( "startRow", E.int startRow )
        , ( "startCol", E.int startCol )
        , ( "context", encodeContext context )
        ]


encodeContext : Context -> E.Value
encodeContext context =
    E.string <|
        case context of
            StrLit ->
                "str"

            ListLit ->
                "list"

            KeyLit ->
                "key"

            TopCtx ->
                "top"


encodeSExpr : SExpr -> E.Value
encodeSExpr sExpr =
    case sExpr of
        List l ->
            E.list encodeSExpr l

        Symbol sym ->
            E.string sym

        Str str ->
            E.object
                [ ( "type", E.string "str" )
                , ( "value", E.string str )
                ]

        Num num ->
            E.float num

        Key k ->
            E.object
                [ ( "type", E.string "key" )
                , ( "value", E.int k )
                ]


parseToJson : String -> E.Value
parseToJson input =
    case parse input of
        Ok parsed ->
            E.object
                [ ( "status", E.string "ok" )
                , ( "parsed", E.list encodeSExpr parsed )
                ]

        Err errs ->
            E.object
                [ ( "status", E.string "err" )
                , ( "error"
                  , case pickErr errs of
                        Just err ->
                            err |> reprErr |> encodeErrRepr

                        Nothing ->
                            E.null
                  )
                ]


parse : String -> Result (List Error) (List SExpr)
parse input =
    Parser.run parser input


parser : Parser (List SExpr)
parser =
    inContext TopCtx <|
        succeed identity
            |= program
            |. end ExpectedExpr


program : Parser (List SExpr)
program =
    sequence
        { start = Token "" Never
        , separator = Token "" Never
        , end = Token "" Never
        , spaces = spaces
        , item = expr
        , trailing = Optional
        }


expr : Parser SExpr
expr =
    oneOf
        [ map List list
        , map Symbol symbol
        , map Num float
        , map Key key
        , map Str string
        ]


float : Parser Float
float =
    Parser.float ExpectedExpr InvalidNumber


symbol : Parser String
symbol =
    variable
        { start = Char.isAlpha
        , inner = Char.isAlphaNum
        , reserved = Set.empty
        , expecting = ExpectedExpr
        }


list : Parser (List SExpr)
list =
    inContext ListLit <|
        sequence
            { start = Token "(" ExpectedExpr
            , separator = Token "" Never
            , end = Token ")" ExpectedListEnd
            , spaces = spaces
            , item = lazy (\_ -> expr)
            , trailing = Optional
            }


key : Parser Int
key =
    inContext KeyLit <|
        (succeed identity
            |. Parser.symbol (Token "@" ExpectedExpr)
            |= variable
                { start = Char.isAlphaNum
                , inner = Char.isAlphaNum
                , reserved = Set.empty
                , expecting = ExpectedKeyName
                }
            |> andThen
                (\keyname ->
                    case keyNameToCode keyname of
                        Just code ->
                            succeed code

                        Nothing ->
                            problem <| InvalidKey keyname
                )
        )



-- STRINGS


string : Parser String
string =
    inContext StrLit <|
        (succeed identity
            |. token (Token "\"" ExpectedExpr)
            |= loop [] stringHelp
            |> andThen
                (\maybe ->
                    case maybe of
                        Just str ->
                            succeed str

                        Nothing ->
                            problem UnexpectedStringEnd
                )
        )


stringHelp : List String -> Parser (Step (List String) (Maybe String))
stringHelp revChunks =
    oneOf
        [ succeed (\chunk -> Loop (chunk :: revChunks))
            |. token (Token "\\" InvalidStringEscape)
            |= oneOf
                [ map (\_ -> "\n") (token (Token "n" InvalidStringEscape))
                , map (\_ -> "\t") (token (Token "t" InvalidStringEscape))
                , map (\_ -> "\"") (token (Token "\"" InvalidStringEscape))
                , map (\_ -> "\\") (token (Token "\\" InvalidStringEscape))
                ]
        , token (Token "\"" UnexpectedStringEnd)
            |> map
                (\_ ->
                    List.reverse revChunks |> String.join "" |> Just |> Done
                )
        , map (\_ -> Done Nothing) (end UnexpectedStringEnd)
        , chompWhile isUninteresting
            |> getChompedString
            |> map (\chunk -> Loop (chunk :: revChunks))
        ]


isUninteresting : Char -> Bool
isUninteresting char =
    char /= '\\' && char /= '"'
