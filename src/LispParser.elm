module LispParser exposing (encodeSExpr, errToString, parse, parseToStringRepr)

import Json.Encode as E
import Keys exposing (keyNameToCode)
import List.Extra
import Parser.Advanced as Parser exposing (..)
import Set


type Problem
    = UnexpectedEOF
    | ExpectedExpr
    | ExpectedList
    | ExpectedListEnd
    | ExpectedSymbol
    | ExpectedString
    | ExpectedNumber
    | ExpectedKey
    | ExpectedKeyName
    | InvalidNumber
    | InvalidString
    | InvalidKey
    | Never


type alias Parser a =
    -- Use `Never` as the context type here because lisp is really simple - it's
    -- generally not too hard to figure out what's going on.
    Parser.Parser Never Problem a


type SExpr
    = List (List SExpr)
    | Symbol String
    | Str String
    | Num Float
    | Key Int


type alias ErrRepr =
    { pos : Maybe ( Int, Int ), msgs : List String }


reprErrs : List (DeadEnd Never Problem) -> ErrRepr
reprErrs errs =
    let
        pos =
            case List.head errs of
                Nothing ->
                    Nothing

                Just { row, col } ->
                    Just ( row, col )
    in
    { pos = pos, msgs = List.map errToString errs }


errToString : DeadEnd Never Problem -> String
errToString err =
    case err.problem of
        UnexpectedEOF ->
            "Reached the end of the input unexpectedly, before the expression was fully parsed"

        ExpectedList ->
            ""

        ExpectedSymbol ->
            ""

        ExpectedString ->
            ""

        ExpectedNumber ->
            ""

        ExpectedKey ->
            ""

        ExpectedExpr ->
            "Expected an expression, like a list: (a b c), string: \"abc\", or number: 123"

        ExpectedKeyName ->
            "Expected the name of a key. There cannot be any spaces between the '@' and the key name"

        ExpectedListEnd ->
            "listend"

        InvalidNumber ->
            ""

        InvalidString ->
            ""

        InvalidKey ->
            ""

        Never ->
            "You should never see this error message"


encodeErrRepr : ErrRepr -> E.Value
encodeErrRepr { pos, msgs } =
    E.object <|
        List.concat
            [ [ ( "msg", E.list E.string msgs )
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

        Err err ->
            Debug.toString <| List.map errToString err


parse : String -> Result (List (DeadEnd Never Problem)) (List SExpr)
parse input =
    Parser.run parser input


parser =
    succeed identity
        |= program
        |. end UnexpectedEOF


program : Parser (List SExpr)
program =
    sequence
        { start = Token "" Never
        , separator = Token "" Never
        , end = Token "" UnexpectedEOF
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
        , expecting = ExpectedSymbol
        }


list : Parser (List SExpr)
list =
    sequence
        { start = Token "(" ExpectedExpr
        , separator = Token "" ExpectedExpr
        , end = Token ")" ExpectedListEnd
        , spaces = spaces
        , item = lazy (\_ -> expr)
        , trailing = Optional
        }


key : Parser Int
key =
    succeed identity
        |. Parser.symbol (Token "@" ExpectedKey)
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
                        problem InvalidKey
            )



-- STRINGS


string : Parser String
string =
    succeed identity
        |. token (Token "\"" ExpectedString)
        |= loop [] stringHelp
        |> andThen
            (\maybe ->
                case maybe of
                    Just str ->
                        succeed str

                    Nothing ->
                        problem UnexpectedEOF
            )


stringHelp : List String -> Parser (Step (List String) (Maybe String))
stringHelp revChunks =
    oneOf
        [ succeed (\chunk -> Loop (chunk :: revChunks))
            |. token (Token "\\" InvalidString)
            |= oneOf
                [ map (\_ -> "\n") (token (Token "n" InvalidString))
                , map (\_ -> "\t") (token (Token "t" InvalidString))
                , map (\_ -> "\"") (token (Token "\"" InvalidString))
                , map (\_ -> "\\") (token (Token "\\" InvalidString))
                ]
        , token (Token "\"" UnexpectedEOF)
            |> map
                (\_ ->
                    List.reverse revChunks |> String.join "" |> Just |> Done
                )
        , map (\_ -> Done Nothing) (end UnexpectedEOF)
        , chompWhile isUninteresting
            |> getChompedString
            |> map (\chunk -> Loop (chunk :: revChunks))
        ]


isUninteresting : Char -> Bool
isUninteresting char =
    char /= '\\' && char /= '"'
