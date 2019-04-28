module Common exposing
    ( Error
    , LocatedNode
    , Location
    , encodeError
    , encodeWithLocation
    , errNode
    , foldlListResult
    , foldrListResult
    , mapListResult
    , mapNode
    )

import Json.Encode as E


type alias Error =
    { loc : Location
    , msg : String
    }


type alias Location =
    { start : ( Int, Int )
    , end : ( Int, Int )
    }


type alias LocatedNode a =
    { loc : Location
    , node : a
    }


encodeWithLocation : Location -> List ( String, E.Value ) -> E.Value
encodeWithLocation loc obj =
    let
        ( startRow, startCol ) =
            loc.start

        ( endRow, endCol ) =
            loc.end
    in
    E.object <|
        List.append obj
            [ ( "startRow", E.int startRow )
            , ( "startCol", E.int startCol )
            , ( "endRow", E.int endRow )
            , ( "endCol", E.int endCol )
            ]


encodeError : Error -> E.Value
encodeError { loc, msg } =
    encodeWithLocation loc [ ( "msg", E.string msg ) ]


mapNode : LocatedNode a -> b -> LocatedNode b
mapNode { loc } b =
    { loc = loc, node = b }


errNode : LocatedNode a -> String -> Error
errNode { loc } msg =
    { msg = msg, loc = loc }


foldlListResult : (a -> b -> Result e b) -> b -> List a -> Result e b
foldlListResult func acc =
    List.foldl (\a -> Result.andThen (func a)) (Ok acc)


foldrListResult : (a -> b -> Result e b) -> b -> List a -> Result e b
foldrListResult func acc =
    List.foldr (\a -> Result.andThen (func a)) (Ok acc)


mapListResult : (a -> Result e b) -> List a -> Result e (List b)
mapListResult func =
    foldrListResult (\a list -> func a |> Result.map (\b -> b :: list)) []
