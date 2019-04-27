module Common exposing
    ( Error
    , LocatedNode
    , errNode
    , foldlListResult
    , mapListResult
    , mapNode
    )


type alias Error =
    { msg : String
    , startPos : ( Int, Int )
    , endPos : ( Int, Int )
    }


type alias LocatedNode a =
    { startPos : ( Int, Int )
    , node : a
    , endPos : ( Int, Int )
    }


mapNode : b -> LocatedNode a -> LocatedNode b
mapNode b { startPos, endPos } =
    { startPos = startPos, node = b, endPos = endPos }


errNode : LocatedNode a -> String -> Error
errNode { startPos, endPos } msg =
    { msg = msg, startPos = startPos, endPos = endPos }


foldlListResult : (a -> b -> Result e b) -> b -> List a -> Result e b
foldlListResult func acc =
    List.foldl (\a -> Result.andThen (func a)) (Ok acc)


foldrListResult : (a -> b -> Result e b) -> b -> List a -> Result e b
foldrListResult func acc =
    List.foldr (\a -> Result.andThen (func a)) (Ok acc)


mapListResult : (a -> Result e b) -> List a -> Result e (List b)
mapListResult func =
    foldrListResult (\a list -> func a |> Result.map (\b -> b :: list)) []
