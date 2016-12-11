module Cards exposing (..)

import Html exposing (..)


type alias Model =
    String


type Msg
    = NoOp


init : ( Model, Cmd Msg )
init =
    ( "Tilman", Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div [] [ text ("Hallo " ++ model) ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
