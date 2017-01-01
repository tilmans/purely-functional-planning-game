module Hello exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Phoenix.Socket
import Phoenix.Channel
import Phoenix.Push
import Json.Encode as JE
import Random exposing (..)
import Navigation exposing (Location)
import Task exposing (Task)


type alias Model =
    { phxSocket : Phoenix.Socket.Socket Msg
    , message : String
    , channel : Maybe String
    , roomID : Maybe String
    , played : Maybe Int
    , name : String
    }


socketServer =
    "ws://localhost:4000/socket/websocket"


cards =
    [ 0, 1, 3, 5, 8, 13 ]


userParams : JE.Value
userParams =
    JE.object [ ( "user_id", JE.string "123" ) ]


joinRoom roomID socket =
    let
        channelID =
            "game:" ++ roomID

        channel =
            Phoenix.Channel.init (channelID)
                |> Phoenix.Channel.withPayload userParams
                |> Phoenix.Channel.onJoin (always (ShowJoinMessage channelID))
                |> Phoenix.Channel.onClose (always (ShowLeaveMessage channelID))

        ( phxSocket, phxCmd ) =
            Phoenix.Socket.join channel socket

        phxSocketListen =
            phxSocket
                |> Phoenix.Socket.on "play.card" channelID VoteFromServer
    in
        (phxSocketListen, Cmd.map PhoenixMsg phxCmd)


init : Location -> ( Model, Cmd Msg )
init location =
    let
        socket =
            Phoenix.Socket.init socketServer
                |> Phoenix.Socket.withDebug
                |> Phoenix.Socket.on "new:msg" "game:*" ReceiveMessage

        id =
            getIdFrom location

        (socketJoined, cmd) =
            case id of
                Nothing ->
                    (socket, Cmd.none)

                Just roomID ->
                    joinRoom roomID socket

        _ =
            Debug.log "Connect" socket
    in
        ( { phxSocket = socketJoined
          , message = ""
          , channel = Nothing
          , roomID = Nothing
          , played = Nothing
          , name = "Tilman"
          }
        , cmd
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
    | Play Int
    | VoteFromServer JE.Value
    | NameChange String
    | UrlChange Location


startform model =
    div []
        [ div []
            [ input [ type_ "text", onInput NameChange, value model.name ] [] ]
        , div []
            [ input [ type_ "text", onInput RoomIDChanged ] []
            , button [ onClick JoinRoom ] [ text "Join Game" ]
            ]
        , div []
            [ button [ onClick CreateRoom ] [ text "Start new Game" ] ]
        ]


card number =
    div [ class "card", onClick (Play number) ] [ text (toString number) ]


gameform =
    div [] (List.map card cards)


view : Model -> Html Msg
view model =
    if model.channel == Nothing then
        startform model
    else
        gameform


getIdFrom location =
    let
        idString =
            String.dropLeft 1 location.search
    in
        if idString == "" then
            Nothing
        else
            Just idString


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "Update" msg
    in
        case msg of
            UrlChange location ->
                let
                    gameid =
                        getIdFrom location
                in
                    case gameid of
                        Nothing ->
                            model ! []

                        Just idvalue ->
                            update JoinChannel { model | roomID = Just idvalue }

            JoinChannel ->
                case model.roomID of
                    Nothing ->
                        ( model, Cmd.none )

                    Just roomID ->
                        let
                            (phxSocket, phxCmd) = joinRoom roomID model.phxSocket
                        in
                            ( { model | message = "Joining", phxSocket = phxSocket }
                            , phxCmd
                            )

            SendMessage ->
                let
                    payload =
                        (JE.object [ ( "user", JE.string model.name ), ( "body", JE.string "Hallo" ) ])

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

            NameChange newName ->
                model ! []

            NewRoom roomID ->
                let
                    newModel =
                        { model | roomID = Just (toString roomID) }

                    ( model_, cmd_ ) =
                        update JoinChannel newModel
                in
                    ( model_, cmd_ )

            Play number ->
                let
                    newmodel =
                        { model | played = Just number }
                in
                    ( newmodel, play newmodel )

            VoteFromServer vote ->
                let
                    _ =
                        Debug.log "Vote" vote
                in
                    model ! []


play : Model -> Cmd Msg
play model =
    case model.channel of
        Nothing ->
            Cmd.none

        Just channel ->
            case model.played of
                Nothing ->
                    Cmd.none

                Just number ->
                    let
                        payload =
                            (JE.object
                                [ ( "user", JE.string model.name )
                                , ( "number", JE.int number )
                                ]
                            )

                        push =
                            Phoenix.Push.init "play.card" channel
                                |> Phoenix.Push.withPayload payload

                        ( phxSocket, phxCmd ) =
                            Phoenix.Socket.push push model.phxSocket
                    in
                        Cmd.map PhoenixMsg phxCmd


subscriptions : Model -> Sub Msg
subscriptions model =
    Phoenix.Socket.listen model.phxSocket PhoenixMsg


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
