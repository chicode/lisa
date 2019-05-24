port module LisaJS exposing (main)

import Dict
import Json.Encode as E
import Keys
import Lisa



{-
   JS:
    function processLisa(s) {
      let out;
      function sub(res) {
        out = res
        app.ports.out.unsubscribe(sub)
      }
      app.ports.out.subscribe(sub)
      app.ports.incoming.send(s)
      return out
    }
-}


type Msg
    = Request String


update : Msg -> () -> ( (), Cmd Msg )
update msg () =
    case msg of
        Request s ->
            ( ()
            , out <|
                Lisa.parseExpressionToJson s
                    { macros = Dict.fromList [ ( "key", Keys.keyMacro ) ] }
            )


main : Program E.Value () Msg
main =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , update = update
        , subscriptions = \_ -> incoming Request
        }


port incoming : (String -> msg) -> Sub msg


port out : E.Value -> Cmd msg
