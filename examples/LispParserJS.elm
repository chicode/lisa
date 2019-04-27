port module LispParserLib exposing (main)

import Json.Encode as E
import LispParser



{-
   JS:
    function parse(s) {
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
                case LispParser.parse s of
                    Ok parsed ->
                        E.list LispParser.encodeSExpr parsed

                    Err err ->
                        E.string "woops"
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
