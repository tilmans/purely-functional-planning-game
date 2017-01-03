module GameUtil exposing (..)

import Json.Encode as JE


userParams : JE.Value
userParams =
    JE.object [ ( "user_id", JE.string "123" ) ]


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
