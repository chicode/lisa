module Lisa.Common exposing
    ( Error
    , LocatedNode
    , Location
    , encodeError
    , encodeResult
    , encodeWithLocation
    , errNode
    , foldlListResult
    , foldrListResult
    , groupListEvery2
    , mapListResult
    , mapNode
    , nonRecovErrNode
    , nonRecovError
    , Recoverable(..)
    )

{-|

@docs Error
@docs LocatedNode
@docs Location
@docs Recoverable(..)
@docs encodeError
@docs encodeResult
@docs encodeWithLocation
@docs errNode
@docs foldlListResult
@docs foldrListResult
@docs groupListEvery2
@docs mapListResult
@docs mapNode
@docs nonRecovErrNode
@docs nonRecovError

-}

import Json.Encode as E


{-| -}
type Recoverable
    = Recoverable
    | Nonrecoverable


{-| -}
type alias Error =
    { recoverable : Recoverable
    , loc : Location
    , msg : String
    }


{-| -}
type alias Location =
    { start : ( Int, Int )
    , end : ( Int, Int )
    }


{-| -}
type alias LocatedNode a =
    { loc : Location
    , node : a
    }


{-| -}
encodeWithLocation : Location -> List ( String, E.Value ) -> E.Value
encodeWithLocation loc obj =
    let
        ( startRow, startCol ) =
            loc.start

        ( endRow, endCol ) =
            loc.end
    in
    E.object <|
        ( "location"
        , E.object
            [ ( "startRow", E.int startRow )
            , ( "startCol", E.int startCol )
            , ( "endRow", E.int endRow )
            , ( "endCol", E.int endCol )
            ]
        )
            :: obj


{-| -}
encodeError : Error -> E.Value
encodeError { recoverable, loc, msg } =
    encodeWithLocation loc
        [ ( "recoverable"
          , case recoverable of
                Recoverable ->
                    E.bool True

                Nonrecoverable ->
                    E.bool False
          )
        , ( "msg", E.string msg )
        ]


{-| -}
encodeResult : (a -> E.Value) -> Result Error a -> E.Value
encodeResult func r =
    case r of
        Ok result ->
            E.object
                [ ( "status", E.string "ok" )
                , ( "result", func result )
                ]

        Err err ->
            E.object
                [ ( "status", E.string "err" )
                , ( "error", encodeError err )
                ]


{-| -}
mapNode : LocatedNode a -> b -> LocatedNode b
mapNode { loc } b =
    { loc = loc, node = b }


{-| -}
errNode : Recoverable -> LocatedNode a -> String -> Error
errNode recov { loc } msg =
    Error recov loc msg


{-| -}
foldlListResult : (a -> b -> Result e b) -> b -> List a -> Result e b
foldlListResult func acc =
    List.foldl (Result.andThen << func) (Ok acc)


{-| -}
foldrListResult : (a -> b -> Result e b) -> b -> List a -> Result e b
foldrListResult func acc =
    List.foldr (Result.andThen << func) (Ok acc)


{-| -}
mapListResult : (a -> Result e b) -> List a -> Result e (List b)
mapListResult func =
    foldlListResult (\a list -> func a |> Result.map (\b -> b :: list)) []
        >> Result.map List.reverse


{-| -}
nonRecovError : Location -> String -> Error
nonRecovError =
    Error Nonrecoverable


{-| -}
nonRecovErrNode : LocatedNode a -> String -> Error
nonRecovErrNode =
    errNode Nonrecoverable


{-| -}
groupListEvery2 : List a -> Result a (List ( a, a ))
groupListEvery2 =
    splitHelp []


splitHelp : List ( a, a ) -> List a -> Result a (List ( a, a ))
splitHelp revList list =
    case list of
        a :: b :: rest ->
            splitHelp (( a, b ) :: revList) rest

        [] ->
            Ok <| List.reverse revList

        a :: [] ->
            Err a
