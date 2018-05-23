module Form.Input exposing (Input, baseInput, checkboxInput, dumpErrors, passwordInput, radioInput, selectInput, textArea, textInput)

{-| Html input view helpers, wired for elm-form validation.

@docs Input

@docs baseInput, textInput, passwordInput, textArea, checkboxInput, selectInput, radioInput

@docs dumpErrors

-}

import Form exposing (FieldState, Form, InputType(..), Msg(Blur, Focus, Input))
import Form.Field as Field exposing (Field, FieldValue(..))
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Maybe exposing (andThen)
import String


{-| An input renders Html from a field state and list of additional attributes.
All input functions using this type alias are pre-wired with event handlers.
-}
type alias Input e a =
    FieldState e a -> List (Attribute Msg) -> Html Msg


{-| Untyped input, first param is `type` attribute.
-}
baseInput : String -> (String -> FieldValue) -> InputType -> Input e String
baseInput t toFieldValue inputType state attrs =
    let
        formAttrs =
            [ type_ t
            , defaultValue (state.value |> Maybe.withDefault "")
            , onInput (toFieldValue >> Input state.path inputType)
            , onFocus (Focus state.path)
            , onBlur (Blur state.path)
            ]
    in
    input (formAttrs ++ attrs) []


{-| Text input.
-}
textInput : Input e String
textInput =
    baseInput "text" String Text


{-| Password input.
-}
passwordInput : Input e String
passwordInput =
    baseInput "password" String Text


{-| Textarea.
-}
textArea : Input e String
textArea state attrs =
    let
        formAttrs =
            [ defaultValue (state.value |> Maybe.withDefault "")
            , onInput (String >> Input state.path Textarea)
            , onFocus (Focus state.path)
            , onBlur (Blur state.path)
            ]
    in
    Html.textarea (formAttrs ++ attrs) []


{-| Select input.
-}
selectInput : List ( String, String ) -> Input e String
selectInput options state attrs =
    let
        formAttrs =
            [ on
                "change"
                (targetValue |> Json.map (String >> Input state.path Select))
            , onFocus (Focus state.path)
            , onBlur (Blur state.path)
            ]

        buildOption ( k, v ) =
            option [ value k, selected (state.value == Just k) ] [ text v ]
    in
    select (formAttrs ++ attrs) (List.map buildOption options)


{-| Checkbox input.
-}
checkboxInput : Input e Bool
checkboxInput state attrs =
    let
        formAttrs =
            [ type_ "checkbox"
            , checked (state.value |> Maybe.withDefault False)
            , onCheck (Bool >> Input state.path Checkbox)
            , onFocus (Focus state.path)
            , onBlur (Blur state.path)
            ]
    in
    input (formAttrs ++ attrs) []


{-| Radio input.
-}
radioInput : String -> Input e String
radioInput value state attrs =
    let
        formAttrs =
            [ type_ "radio"
            , name state.path
            , HtmlAttr.value value
            , checked (state.value == Just value)
            , onFocus (Focus state.path)
            , onBlur (Blur state.path)
            , on
                "change"
                (targetValue |> Json.map (String >> Input state.path Radio))
            ]
    in
    input (formAttrs ++ attrs) []


{-| Dump all form errors in a `<pre>` tag. Useful for debugging.
-}
dumpErrors : Form e o -> Html msg
dumpErrors form =
    let
        errorToString error =
            case error of
                Empty ->
                    "Empty"

                InvalidString ->
                    "InvalidString"

                InvalidEmail ->
                    "InvalidEmail"

                InvalidFormat ->
                    "InvalidFormat"

                InvalidInt ->
                    "InvalidInt"

                InvalidFloat ->
                    "InvalidFloat"

                InvalidBool ->
                    "InvalidBool"

                InvalidDate ->
                    "InvalidDate"

                SmallerIntThan v ->
                    "SmallerIntThan " ++ String.fromInt v

                GreaterIntThan v ->
                    "GreaterIntThan " ++ String.fromInt v

                SmallerFloatThan v ->
                    "SmallerFloatThan " ++ String.fromFloat v

                GreaterFloatThan v ->
                    "GreaterFloatThan " ++ String.fromFloat v

                ShorterStringThan v ->
                    "ShorterStringThan " ++ String.fromInt v

                LongerStringThan v ->
                    "LongerStringThan " ++ String.fromInt v

                NotIncludedIn ->
                    "NotIncludedIn"

                CustomError _ ->
                    "CustomError"

        line ( name, error ) =
            name ++ ": " ++ errorToString error

        content =
            Form.getErrors form |> List.map line |> String.join "\n"
    in
    pre [] [ text content ]
