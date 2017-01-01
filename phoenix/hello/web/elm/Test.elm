module Hello exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Phoenix.Socket exposing (Socket)
import Phoenix.Channel
import Phoenix.Push
import Json.Encode as JE
import Json.Decode as JD exposing (field)
import Random exposing (..)
import Navigation exposing (Location)
import Task exposing (Task)
import Dict exposing (Dict)
import Maybe exposing (withDefault)
import NameInput


type alias Model =
    { phxSocket : Phoenix.Socket.Socket Msg
    , message : String
    , channel : Maybe String
    , roomID : Maybe String
    , played : Maybe Int
    , name : NameInput.Model
    , votes : Dict String Int
    , state : ViewState
    }


type alias Vote =
    { user : String
    , vote : Int
    }


socketServer =
    "ws://localhost:4000/socket/websocket"


cards =
    [ 0, 1, 3, 5, 8, 13 ]


type Msg
    = PhoenixMsg (Phoenix.Socket.Msg Msg)
    | ReceiveMessage JE.Value
    | ShowLeaveMessage String
    | ShowJoinMessage String
    | JoinChannel
    | JoinRoom
    | CreateRoom
    | RoomIDChanged String
    | NewRoom Int
    | Play Int
    | VoteFromServer JE.Value
    | UrlChange Location
    | NameInputMsg NameInput.Msg


type ViewState
    = NameInput
    | RoomInput
    | Game


userParams : JE.Value
userParams =
    JE.object [ ( "user_id", JE.string "123" ) ]


init : Location -> ( Model, Cmd Msg )
init location =
    let
        socket =
            Phoenix.Socket.init socketServer
                |> Phoenix.Socket.withDebug
                |> Phoenix.Socket.on "new:msg" "game:*" ReceiveMessage

        id =
            getIdFrom location.search

        ( socketJoined, cmd ) =
            case id of
                Nothing ->
                    ( socket, Cmd.none )

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
          , name = NameInput.initialModel
          , votes = Dict.empty
          , state = NameInput
          }
        , cmd
        )


startform : Model -> Html Msg
startform model =
    div []
        [ div []
            [ input [ type_ "text", onInput RoomIDChanged ] []
            , button [ onClick JoinRoom ] [ text "Join Game" ]
            ]
        , div []
            [ button [ onClick CreateRoom ] [ text "Start new Game" ] ]
        ]


card : Int -> Html Msg
card number =
    div [ class "card", onClick (Play number) ] [ text (toString number) ]


vote : ( String, Int ) -> Html Msg
vote ( user, number ) =
    div []
        [ text (user ++ ": " ++ toString (number))
        ]


gameform : Model -> Html Msg
gameform model =
    let
        gameurl =
            case model.roomID of
                Nothing ->
                    "/hello?"

                Just id ->
                    "/hello?" ++ id
    in
        div []
            [ a [ href gameurl ] [ text "Link to room" ]
            , div [] [ text "Played", div [] (List.map vote (Dict.toList model.votes)) ]
            , div [] (List.map card cards)
            ]


view : Model -> Html Msg
view model =
    case model.state of
        NameInput ->
            Html.map NameInputMsg (NameInput.view model.name)

        RoomInput ->
            startform model

        Game ->
            gameform model


getIdFrom : String -> Maybe String
getIdFrom location =
    let
        idString =
            String.dropLeft 1 location
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
                        getIdFrom location.search
                in
                    case gameid of
                        Nothing ->
                            model ! []

                        Just idvalue ->
                            update JoinChannel { model | roomID = Just idvalue }

            JoinChannel ->
                case model.roomID of
                    Nothing ->
                        model ! []

                    Just roomID ->
                        let
                            ( phxSocket, phxCmd ) =
                                joinRoom roomID model.phxSocket
                        in
                            ( { model | message = "Joining", phxSocket = phxSocket }
                            , phxCmd
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

                    voteDict =
                        case JD.decodeValue decodeVote vote of
                            Ok vote ->
                                Dict.insert vote.user vote.vote model.votes

                            Err error ->
                                model.votes
                in
                    ( { model | votes = voteDict }, Cmd.none )

            NameInputMsg msg ->
                let
                    ( updatedNameModel, nameCmd ) =
                        NameInput.update msg model.name

                    nextState =
                        if updatedNameModel.set then
                            RoomInput
                        else
                            NameInput
                in
                    ( { model | name = updatedNameModel, state = nextState }, Cmd.map NameInputMsg nameCmd )


decodeVote : JD.Decoder Vote
decodeVote =
    JD.map2 Vote
        (field "user" JD.string)
        (field "number" JD.int)


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
                                [ ( "user", JE.string model.name.name )
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


joinRoom : String -> Socket Msg -> ( Socket Msg, Cmd Msg )
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
        ( phxSocketListen, Cmd.map PhoenixMsg phxCmd )
