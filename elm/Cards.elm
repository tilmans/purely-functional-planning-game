module Cards exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import WebSocket


type alias Model =
    { input : String
    , messages : List String
    }


type Msg
    = NewMessage String
    | Input String
    | Send


server =
    "ws://localhost:9160"


init : ( Model, Cmd Msg )
init =
    ( Model "Tilman" [], Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewMessage message ->
            ( { model | messages = message :: model.messages }, Cmd.none )

        Input string ->
            ( model, Cmd.none )

        Send ->
            ( model, connect model )


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen server NewMessage


view : Model -> Html Msg
view model =
    div []
        [ div [] (List.map viewMessage model.messages)
        , input [ onInput Input ] []
        , button [ onClick Send ] [ text "Send" ]
        ]


viewMessage : String -> Html msg
viewMessage msg =
    div [] [ text msg ]


connect model =
    WebSocket.send server ("Hi! I am " ++ model.input)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
