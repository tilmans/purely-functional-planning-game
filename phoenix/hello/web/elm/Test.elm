module Hello exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Phoenix.Socket
import Phoenix.Channel
import Phoenix.Push
import Json.Encode as JE


type alias Model =
    { phxSocket : Phoenix.Socket.Socket Msg
    , message : String
    }


socketServer =
    "ws://localhost:4000/socket/websocket"


userParams : JE.Value
userParams =
    JE.object [ ( "user_id", JE.string "123" ) ]


init : ( Model, Cmd Msg )
init =
    let
        socket =
            Phoenix.Socket.init socketServer
                |> Phoenix.Socket.withDebug
                |> Phoenix.Socket.on "new:msg" "game:*" ReceiveMessage

        _ =
            Debug.log "Connect" socket
    in
        ( { phxSocket = socket
          , message = ""
          }
        , Cmd.none
        )


type Msg
    = PhoenixMsg (Phoenix.Socket.Msg Msg)
    | ReceiveMessage JE.Value
    | SendMessage
    | ShowLeaveMessage String
    | ShowJoinMessage String
    | JoinChannel


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text model.message ]
        , div [ onClick JoinChannel ] [ text "Join me" ]
        , div [ onClick SendMessage ] [ text "Send Message" ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "Update" msg
    in
        case msg of
            JoinChannel ->
                let
                    channel =
                        Phoenix.Channel.init "game:1"
                            |> Phoenix.Channel.withPayload userParams
                            |> Phoenix.Channel.onJoin (always (ShowJoinMessage "game:1"))
                            |> Phoenix.Channel.onClose (always (ShowLeaveMessage "game:1"))

                    ( phxSocket, phxCmd ) =
                        Phoenix.Socket.join channel model.phxSocket
                in
                    ( { model | message = "Joining", phxSocket = phxSocket }
                    , Cmd.map PhoenixMsg phxCmd
                    )

            SendMessage ->
                let
                    payload =
                        (JE.object [ ( "user", JE.string "user" ), ( "body", JE.string "Hallo" ) ])

                    push =
                        Phoenix.Push.init "new.msg" "game:1"
                            |> Phoenix.Push.withPayload payload

                    ( phxSocket, phxCmd ) =
                        Phoenix.Socket.push push model.phxSocket
                in
                    ( { model | message = "Sending", phxSocket = phxSocket }
                    , Cmd.map PhoenixMsg phxCmd
                    )

            ReceiveMessage msg ->
                ( model, Cmd.none )

            ShowLeaveMessage text ->
                ( { model | message = "Left Channel" }, Cmd.none )

            ShowJoinMessage text ->
                ( { model | message = "Joined Channel" }, Cmd.none )

            PhoenixMsg msg ->
                let
                    ( phxSocket, phxCmd ) =
                        Phoenix.Socket.update msg model.phxSocket

                    _ =
                        Debug.log "Message" msg
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
