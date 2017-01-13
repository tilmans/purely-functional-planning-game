module Play exposing (..)

import Html exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Phoenix.Socket exposing (Socket)
import Phoenix.Channel
import Phoenix.Push
import Json.Decode as JD exposing (field)
import Json.Encode as JE
import Random exposing (..)
import Navigation exposing (Location)
import Task exposing (Task)
import Dict exposing (Dict)
import Maybe exposing (withDefault)
import Color exposing (rgb)
import AFrame exposing (scene, entity)
import AFrame.Primitives as AP exposing (..)
import AFrame.Primitives.Attributes as AA exposing (..)
import AFrame.Primitives.Camera exposing (..)
import AFrame.Primitives.Cursor exposing (..)


{--TODO
* Check that the room is an Int
* Resend votes from server on connect
* Make sure the game link works
--}


type alias Vote =
    { user : String
    , vote : Int
    }


socketServer : Location -> String
socketServer location =
    let
        server =
            location.hostname

        protocol =
            if location.protocol == "http:" then
                "ws"
            else
                "wss"
    in
        protocol ++ "://" ++ server ++ ":" ++ location.port_ ++ "/socket/websocket"


cards =
    [ 0, 1, 2, 3, 5, 8, 13 ]


type Msg
    = PhoenixMsg (Phoenix.Socket.Msg Msg)
    | ShowLeaveMessage String
    | ShowJoinMessage String
    | VoteFromServer JE.Value
    | JoinRoom
    | CreateRoom
    | RoomIDChanged String
    | NewRoom Int
    | Play Int
    | NameChange String
    | UrlChange Location
    | SetName
    | ListUpdate JE.Value
    | CardSelected Int


type State
    = NameInput
    | RoomInput
    | Playing


type alias Model =
    { phxSocket : Phoenix.Socket.Socket Msg
    , message : String
    , channel : Maybe String
    , roomID : Maybe String
    , played : Maybe Int
    , name : Maybe String
    , votes : List Vote
    , state : State
    }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        ( name, id ) =
            getIdFrom location.search

        initsocket =
            Phoenix.Socket.init (socketServer location)
                |> Phoenix.Socket.withDebug
                |> Phoenix.Socket.on "list" "game:*" ListUpdate

        model =
            { phxSocket = initsocket
            , message = ""
            , channel = Nothing
            , roomID = id
            , played = Nothing
            , name = name
            , votes = []
            , state = NameInput
            }

        ( nextState, socket, cmd ) =
            progressState model
    in
        ( { model | state = nextState, phxSocket = socket }
        , cmd
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "Update" msg
    in
        case msg of
            UrlChange location ->
                {--TODO: Change the room? --}
                model ! []

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
                let
                    ( nextState, socket, cmd ) =
                        progressState model
                in
                    { model | phxSocket = socket, state = nextState } ! [ cmd ]

            CreateRoom ->
                ( model, Random.generate NewRoom (Random.int 0 99999999) )

            NewRoom roomID ->
                let
                    newModel =
                        { model | roomID = Just (toString roomID) }

                    ( joinedModel, cmd ) =
                        update JoinRoom newModel
                in
                    joinedModel ! [ cmd ]

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
                { model | name = Just newName } ! []

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

                    votes =
                        case JD.decodeValue decodeVote vote of
                            Ok vote ->
                                vote :: List.filter (notUser vote.user) model.votes

                            Err error ->
                                model.votes
                in
                    { model | votes = votes } ! []

            SetName ->
                let
                    ( nextState, socket, cmd ) =
                        progressState model
                in
                    { model | state = nextState, phxSocket = socket } ! [ cmd ]

            ListUpdate msg ->
                model ! []

            CardSelected card ->
                { model | played = Just card } ! []


progressState : Model -> ( State, Phoenix.Socket.Socket Msg, Cmd Msg )
progressState model =
    case model.state of
        NameInput ->
            if model.name == Nothing then
                ( NameInput, model.phxSocket, Cmd.none )
            else if model.roomID == Nothing then
                ( RoomInput, model.phxSocket, Cmd.none )
            else
                let
                    ( socket, cmd ) =
                        connectSocket model
                in
                    ( Playing, socket, cmd )

        RoomInput ->
            if model.roomID == Nothing then
                ( RoomInput, model.phxSocket, Cmd.none )
            else
                let
                    ( socket, cmd ) =
                        connectSocket model
                in
                    ( Playing, socket, cmd )

        Playing ->
            let
                ( socket, cmd ) =
                    connectSocket model
            in
                ( Playing, socket, cmd )


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



{--User Interface --}


startform : Model -> Html Msg
startform model =
    div []
        [ div []
            [ input [ HA.type_ "text", onInput RoomIDChanged ] []
            , button [ onClick JoinRoom ] [ text "Join Game" ]
            ]
        , div []
            [ button [ onClick CreateRoom ] [ text "Start new Game" ] ]
        ]


card : Int -> Html Msg
card number =
    div [ class "card", onClick (Play number) ] [ text (toString number) ]


availableCards : Html Msg
availableCards =
    div [ class "available-cards" ] (List.map card cards)


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
            , div []
                [ h2 [] [ text "Played Cards" ]
                , playedCards model
                ]
            , div []
                [ h2 [] [ text "Cards" ]
                , availableCards
                ]
            ]


playedCards : Model -> Html Msg
playedCards model =
    case model.votes of
        [] ->
            div [ class "played-cards-placeholder" ] [ text "No Votes yet" ]

        _ ->
            div [ class "played-cards" ] (List.map vote model.votes)


vote : Vote -> Html Msg
vote vote =
    div [ class "card" ]
        [ text (vote.user ++ ": " ++ toString (vote.vote))
        ]


nameform : Model -> Html Msg
nameform model =
    Html.form [ onSubmit SetName ]
        [ input [ HA.type_ "text", onInput NameChange, value (withDefault "" model.name) ] []
        , button [ onClick SetName ] [ text "Join Game" ]
        ]


cardImage : Maybe Int -> Int -> Int -> Html Msg
cardImage selection index number =
    let
        xpos =
            toFloat (index - 3)

        ypos =
            case selection of
                Nothing ->
                    1

                Just sel ->
                    if number == sel then
                        1.5
                    else
                        1

        cardpos =
            position xpos ypos -5
    in
        image
            [ AA.src ("#c" ++ (toString number))
            , cardpos
            , onClick (CardSelected number)
            ]
            []


cardAssets : Int -> Html msg
cardAssets number =
    let
        nS =
            toString number
    in
        img [ id ("c" ++ nS), HA.src ("/images/" ++ nS ++ ".png") ] []


aframeScene : Model -> Html Msg
aframeScene model =
    scene
        [ AA.vrmodeui True ]
        ([ sky
            [ color (rgb 0 255 0) ]
            []
         , camera [ position 0 0 0 ] [ cursor [ fuse True ] [] ]
         ]
            ++ (List.map cardAssets cards)
            ++ (List.indexedMap (cardImage model.played) cards)
        )


view : Model -> Html Msg
view model =
    case model.state of
        NameInput ->
            nameform model

        RoomInput ->
            startform model

        Playing ->
            aframeScene model



{--Utilities --}


decodeVote : JD.Decoder Vote
decodeVote =
    JD.map2 Vote
        (field "user" JD.string)
        (field "number" JD.int)


play : Model -> Cmd Msg
play model =
    if model.name == Nothing then
        Cmd.none
    else
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
                                    [ ( "user", JE.string (withDefault "no name" model.name) )
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


userParams : JE.Value
userParams =
    JE.object [ ( "user_id", JE.string "123" ) ]


getIdFrom : String -> ( Maybe String, Maybe String )
getIdFrom location =
    let
        string =
            String.dropLeft 1 location

        subs =
            String.split "&" string

        name =
            List.foldr (extract "name") Nothing subs

        id =
            List.foldr (extract "room") Nothing subs
    in
        ( name, id )


connectSocket : Model -> ( Phoenix.Socket.Socket Msg, Cmd Msg )
connectSocket model =
    let
        channelID =
            "game:" ++ (withDefault "" model.roomID)

        channel =
            Phoenix.Channel.init (channelID)
                |> Phoenix.Channel.withPayload userParams
                |> Phoenix.Channel.onJoin (always (ShowJoinMessage channelID))
                |> Phoenix.Channel.onClose (always (ShowLeaveMessage channelID))

        ( phxSocket, phxCmd ) =
            Phoenix.Socket.join channel model.phxSocket

        phxSocketListen =
            phxSocket
                |> Phoenix.Socket.on "play.card" channelID VoteFromServer
    in
        ( phxSocketListen, Cmd.map PhoenixMsg phxCmd )


notUser : String -> Vote -> Bool
notUser user vote =
    vote.user /= user


extract : String -> String -> Maybe String -> Maybe String
extract lookFor values accum =
    let
        subs =
            String.split "=" values

        key =
            withDefault "" (List.head subs)
    in
        if key == lookFor then
            List.head (withDefault [] (List.tail subs))
        else
            accum
