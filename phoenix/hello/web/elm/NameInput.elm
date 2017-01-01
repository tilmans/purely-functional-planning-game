module NameInput exposing (Model, Msg, update, initialModel, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


type alias Model =
    { name : String
    , set : Bool
    }


initialModel =
    Model "" False


type Msg
    = NameChange String
    | NameAccept


view model =
    div []
        [ input [ type_ "text", onInput NameChange, value model.name ] []
        , button [ onClick NameAccept ] [ text "Set Name" ]
        ]


update message model =
    case message of
        NameChange name ->
            ( { model | name = name }, Cmd.none )

        NameAccept ->
            ( { model | set = True }, Cmd.none )
