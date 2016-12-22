module Hello exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Phoenix.Socket
import Phoenix.Channel
import Phoenix.Push
import Json.Encode as JE
import Random exposing (..)


type alias Model =
    { phxSocket : Phoenix.Socket.Socket Msg
    , message : String
    , channel : Maybe String
    , roomID : Maybe String
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
          , channel = Nothing
          , roomID = Nothing
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
    | JoinRoom
    | CreateRoom
    | RoomIDChanged String
    | NewRoom Int


startform =
    [ div []
        [ input [ type_ "text", onInput RoomIDChanged ] []
        , button [ onClick JoinRoom ] [ text "Join Game" ]
        ]
    , div []
        [ button [ onClick CreateRoom ] [ text "Start new Game" ] ]
    ]


view : Model -> Html Msg
view model =
    if model.channel == Nothing then
        div [] startform
    else
        (div []
            [ div [] [ text model.message ]
            , div [ onClick JoinChannel ] [ text "Join me" ]
            , div [ onClick SendMessage ] [ text "Send Message" ]
            ]
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "Update" msg
    in
        case msg of
            JoinChannel ->
                case model.roomID of
                    Nothing ->
                        ( model, Cmd.none )

                    Just roomID ->
                        let
                            channelID =
                                "game:" ++ roomID

                            channel =
                                Phoenix.Channel.init (channelID)
                                    |> Phoenix.Channel.withPayload userParams
                                    |> Phoenix.Channel.onJoin (always (ShowJoinMessage channelID))
                                    |> Phoenix.Channel.onClose (always (ShowLeaveMessage channelID))

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
                ( { model | message = "Joined Channel", channel = Just text }, Cmd.none )

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

            JoinRoom ->
                ( model, Cmd.none )

            CreateRoom ->
                ( model, Random.generate NewRoom (Random.int 0 99999999) )

            RoomIDChanged newID ->
                let
                    roomID =
                        if newID == "" then
                            Nothing
                        else
                            Just newID
                in
                    ( { model | roomID = roomID }, Cmd.none )

            NewRoom roomID ->
                let
                    newModel =
                        { model | roomID = Just (toString roomID) }

                    ( model_, cmd_ ) =
                        update JoinChannel newModel
                in
                    ( model_, cmd_ )


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
