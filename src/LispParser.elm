module LispParser exposing
    ( encodeSExpr
    , errorToString
    , maybeErrorToString
    , parse
    , parseToStringRepr
    )

import Json.Encode as E
import Keys exposing (keyNameToCode)
import List.Extra
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
    { pos : Maybe ( Int, Int ), msg : String }


reprErrs : List Error -> ErrRepr
reprErrs errs =
    let
        err =
            pickErr errs

        pos =
            Maybe.map (\{ row, col } -> ( row, col )) err
    in
    { pos = pos, msg = maybeErrorToString err }


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


maybeErrorToString : Maybe Error -> String
maybeErrorToString err =
    err |> Maybe.map errorToString |> Maybe.withDefault ""


encodeErrRepr : ErrRepr -> E.Value
encodeErrRepr { pos, msg } =
    E.object <|
        List.concat
            [ [ ( "msg", E.string msg )
              ]
            , case pos of
                Nothing ->
                    []

                Just ( col, row ) ->
                    [ ( "col", E.int col )
                    , ( "row", E.int row )
                    ]
            ]


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


parseToStringRepr : String -> String
parseToStringRepr input =
    case parse input of
        Ok parsed ->
            E.list encodeSExpr parsed |> E.encode 2

        Err errs ->
            pickErr errs |> maybeErrorToString


parse : String -> Result (List Error) (List SExpr)
parse input =
    Parser.run parser input


parser =
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
        [ map Str string
        , map Symbol symbol
        , map List list
        , map Num float
        , map Key key
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
