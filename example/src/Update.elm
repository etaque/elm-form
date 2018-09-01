module Update exposing (init, update)

import Form exposing (Form)
import Model exposing (..)


init : Model
init =
    { form = Form.initial initialFields validate, userMaybe = Nothing }


update : Msg -> Model -> Model
update msg ({ form } as model) =
    case msg of
        NoOp ->
            model

        FormMsg formMsg ->
            case ( formMsg, Form.getOutput form ) of
                ( Form.Submit, Just user ) ->
                    { model | userMaybe = Just user }

                _ ->
                    { model | form = Form.update validate formMsg form }
