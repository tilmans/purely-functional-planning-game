module Cards exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import WebSocket
import String exposing (startsWith, dropLeft, toInt)


type alias Model =
    { name : Maybe String
    , connected : Bool
    , vote : Maybe Int
    }


type Msg
    = NewMessage String
    | Input String
    | Send
    | Vote Int


server =
    "ws://localhost:9160"


init : ( Model, Cmd Msg )
init =
    ( Model (Just "Tilman") False Nothing, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewMessage message ->
            let
                _ =
                    Debug.log "Message" message
            in
                if startsWith "Welcome!" message then
                    ( { model | connected = True }, Cmd.none )
                else if startsWith "Voted " message then
                    let
                        vote =
                            case toInt (dropLeft 6 message) of
                                Ok r ->
                                    Just r

                                Err _ ->
                                    Nothing
                    in
                        ( { model | vote = vote }, Cmd.none )
                else
                    ( model, Cmd.none )

        Input string ->
            ( model, Cmd.none )

        Send ->
            ( model, connect model )

        Vote vote ->
            ( model, send vote )


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen server NewMessage


view : Model -> Html Msg
view model =
    div [] (cardsOrVote model)


cardsOrVote model =
    if model.connected then
        case model.vote of
            Just vote ->
                [ div [] [ text (toString vote) ] ]

            Nothing ->
                [ div [ onClick (Vote 0) ] [ text "0" ]
                , div [ onClick (Vote 1) ] [ text "1" ]
                ]
    else
        [ input [ onInput Input ] []
        , button [ onClick Send ] [ text "Send" ]
        ]


viewMessage : String -> Html msg
viewMessage msg =
    div [] [ text msg ]


connect : Model -> Cmd Msg
connect model =
    case model.name of
        Just name ->
            WebSocket.send server ("Hi! I am " ++ name)

        _ ->
            Cmd.none


send : Int -> Cmd Msg
send vote =
    WebSocket.send server ("Vote " ++ (toString vote))


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
