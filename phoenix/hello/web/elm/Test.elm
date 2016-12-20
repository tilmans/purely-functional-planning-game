module Hello exposing (..)

import Html exposing (..)
import Phoenix.Socket
import Phoenix.Channel
import Phoenix.Push


type alias Model =
    { phxSocket : Phoenix.Socket.Socket Msg
    }


socketServer =
    "ws://localhost:4000/socket/websocket"


initSocket =
    Phoenix.Socket.init socketServer
        |> Phoenix.Socket.withDebug



--- |> Phoenix.Socket.on "new:msg" "game:*" ReceiveMessage


init : ( Model, Cmd Msg )
init =
    let
        socket =
            initSocket

        channel =
            Phoenix.Channel.init "game:1"

        ( phxSocket, phxCmd ) =
            Phoenix.Socket.join channel socket
    in
        ( { phxSocket = initSocket
          }
        , Cmd.none
        )


type Msg
    = UpdateSomething
    | DoSomethingElse
    | PhoenixMsg (Phoenix.Socket.Msg Msg)
    | ReceiveMessage Msg


view : Model -> Html Msg
view model =
    div []
        [ text "Hallo" ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateSomething ->
            ( model, Cmd.none )

        DoSomethingElse ->
            ( model, Cmd.none )

        ReceiveMessage msg ->
            ( model, Cmd.none )

        PhoenixMsg msg ->
            let
                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.update msg model.phxSocket
            in
                ( { model | phxSocket = phxSocket }
                , Cmd.map PhoenixMsg phxCmd
                )


subscriptions : Model -> Sub Msg
subscriptions model =
    Phoenix.Socket.listen model.phxSocket PhoenixMsg


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
