module Form exposing (Msg(..), Form, FieldState, initial, update, getFieldAsString, getFieldAsBool, getFocus, getErrors, isSubmitted, getOutput, getChangedFields)

{-| Simple forms made easy: A Dict implementation of the core `Json.Decode` API,
with state lifecycle and input helpers for the views.

# Types
@docs Msg, Form, FieldState

# Init/update lifecyle
@docs initial, update

# Field state accessors
@docs getFieldAsString, getFieldAsBool

# Global state accessors
@docs getFocus, isSubmitted, getErrors, getOutput, getChangedFields
-}

import Dict exposing (Dict)
import Result
import String
import Set exposing (Set)
import String
import Form.Error as Error exposing (..)
import Form.Field as Field exposing (..)
import Form.Validate as Validate exposing (Validation)


{-| Form to embed in your model. Type parameters are:

 * `customError` - a custom error type to extend built-in errors (set to `()` if you don't need it)
 * `output` - the type of the validation output.
-}
type Form customError output
    = F (Model customError output)


{-| Private
-}
type alias Model customError output =
    { fields : Field
    , focus : Maybe String
    , dirtyFields : Set String
    , changedFields : Set String
    , isSubmitted : Bool
    , output : Maybe output
    , errors : Error customError
    , validation : Validation customError output
    }


{-| Initial form state. See `Form.Field` for initial fields, and `Form.Validate` for validation.
-}
initial : List ( String, Field ) -> Validation e output -> Form e output
initial initialFields validation =
    let
        model =
            { fields = group initialFields
            , focus = Nothing
            , dirtyFields = Set.empty
            , changedFields = Set.empty
            , isSubmitted = False
            , output = Nothing
            , errors = GroupErrors Dict.empty
            , validation = validation
            }
    in
        F (updateValidate model)


{-| Field state containing all necessary data for view and update,
can be retrived with `Form.getFieldAsString` or `Form.getFieldAsBool`.

 * `path` - qualified path of the field in the form, with dots for nested fields (`field.subfield`)
 * `value` - a `Maybe` of the requested type
 * `error` - a `Maybe` of the field error
 * `liveError` - same but with added logic for live validation
    (see [`getLiveErrorAt`](https://github.com/etaque/elm-simple-form/blob/master/src/Form.elm) impl)
 * `isDirty` - if the field content has been changed since last validation
 * `isChanged` - if the field value has changed since last init/reset
 * `hasFocus` - if the field is currently focused
-}
type alias FieldState e a =
    { path : String
    , value : Maybe a
    , error : Maybe (Error e)
    , liveError : Maybe (Error e)
    , isDirty : Bool
    , isChanged : Bool
    , hasFocus : Bool
    }


{-| Get field state at path, with value as a `String`.
-}
getFieldAsString : String -> Form e o -> FieldState e String
getFieldAsString =
    getField getStringAt


{-| Get field state at path, with value as a `Bool`.
-}
getFieldAsBool : String -> Form e o -> FieldState e Bool
getFieldAsBool =
    getField getBoolAt


getField : (String -> Form e o -> Maybe a) -> String -> Form e o -> FieldState e a
getField getValue path form =
    { path = path
    , value = getValue path form
    , error = getErrorAt path form
    , liveError = getLiveErrorAt path form
    , isDirty = isDirtyAt path form
    , isChanged = isChangedAt path form
    , hasFocus = getFocus form == Just path
    }


{-| Form messages for `update`.
-}
type Msg
    = NoOp
    | Focus String
    | Blur String
    | Input String Field
    | Submit
    | Validate
    | Reset (List ( String, Field ))


{-| Update form state with the given message
-}
update : Msg -> Form e output -> Form e output
update msg (F model) =
    case msg of
        NoOp ->
            F model

        Focus name ->
            let
                newModel =
                    { model | focus = Just name }
            in
                F newModel

        Blur name ->
            let
                newDirtyFields =
                    Set.remove name model.dirtyFields

                newModel =
                    { model | focus = Nothing, dirtyFields = newDirtyFields }
            in
                F (updateValidate newModel)

        Input name field ->
            let
                newFields =
                    setFieldAt name field (F model)

                isDirty =
                    case field of
                        Text _ ->
                            True

                        Textarea _ ->
                            True

                        _ ->
                            False

                newDirtyFields =
                    if isDirty then
                        Set.insert name model.dirtyFields
                    else
                        model.dirtyFields

                newChangedFields =
                    Set.insert name model.changedFields

                newModel =
                    { model
                        | fields = newFields
                        , dirtyFields = newDirtyFields
                        , changedFields = newChangedFields
                    }
            in
                F (updateValidate newModel)

        Submit ->
            let
                validatedModel =
                    updateValidate model
            in
                F { validatedModel | isSubmitted = True }

        Validate ->
            F (updateValidate model)

        Reset fields ->
            let
                newModel =
                    { model
                        | fields = group fields
                        , dirtyFields = Set.empty
                        , changedFields = Set.empty
                        , isSubmitted = False
                    }
            in
                F (updateValidate newModel)


updateValidate : Model e o -> Model e o
updateValidate model =
    case model.validation model.fields of
        Ok output ->
            { model
                | errors =
                    GroupErrors Dict.empty
                , output = Just output
            }

        Err error ->
            { model
                | errors =
                    error
                , output = Nothing
            }


getFieldAt : String -> Form e o -> Maybe Field
getFieldAt qualifiedName (F model) =
    let
        walkPath name maybeField =
            maybeField `Maybe.andThen` Field.at name
    in
        List.foldl walkPath (Just model.fields) (String.split "." qualifiedName)


getStringAt : String -> Form e o -> Maybe String
getStringAt name form =
    getFieldAt name form `Maybe.andThen` asString


getBoolAt : String -> Form e o -> Maybe Bool
getBoolAt name form =
    getFieldAt name form `Maybe.andThen` asBool


setFieldAt : String -> Field -> Form e o -> Field
setFieldAt qualifiedName field (F model) =
    let
        walkPath path maybeNode =
            case path of
                name :: rest ->
                    let
                        node =
                            Maybe.withDefault (Group Dict.empty) maybeNode

                        childField =
                            walkPath rest (Field.at name node)
                    in
                        merge (Group (Dict.fromList [ ( name, childField ) ])) node

                [] ->
                    field
    in
        walkPath (String.split "." qualifiedName) (Just model.fields)


{-| Get form output, in case of validation success.
-}
getOutput : Form e o -> Maybe o
getOutput (F model) =
    model.output


{-| Get form submission state. Useful to show errors on unchanged fields.
-}
isSubmitted : Form e o -> Bool
isSubmitted (F model) =
    model.isSubmitted


{-| Get list of errors on qualified paths.
-}
getErrors : Form e o -> List ( String, Error e )
getErrors (F model) =
    let
        mapGroupItem path ( name, error ) =
            walkTree (path ++ [ name ]) error

        walkTree path node =
            case node of
                GroupErrors errors ->
                    List.concatMap (mapGroupItem path) (Dict.toList errors)

                _ ->
                    [ ( String.join "." path, node ) ]
    in
        walkTree [] model.errors


getErrorAt : String -> Form e o -> Maybe (Error e)
getErrorAt qualifiedName (F model) =
    let
        walkPath path maybeNode =
            case path of
                name :: rest ->
                    maybeNode
                        `Maybe.andThen`
                            \error ->
                                case error of
                                    GroupErrors _ ->
                                        walkPath rest (Error.getAt name error)

                                    _ ->
                                        Just error

                [] ->
                    maybeNode
    in
        walkPath (String.split "." qualifiedName) (Just model.errors)


getLiveErrorAt : String -> Form e o -> Maybe (Error e)
getLiveErrorAt name form =
    if isSubmitted form || (isChangedAt name form && not (isDirtyAt name form)) then
        getErrorAt name form
    else
        Nothing


isChangedAt : String -> Form e o -> Bool
isChangedAt qualifiedName (F model) =
    Set.member qualifiedName model.changedFields


isDirtyAt : String -> Form e o -> Bool
isDirtyAt qualifiedName (F model) =
    Set.member qualifiedName model.dirtyFields


{-| Return currently focused field, if any.
-}
getFocus : Form e o -> Maybe String
getFocus (F model) =
    model.focus


merge : Field -> Field -> Field
merge v1 v2 =
    case ( v1, v2 ) of
        ( Group g1, Group g2 ) ->
            Group (Dict.union g1 g2)

        _ ->
            v1


{-| Get set of changed fields.
-}
getChangedFields : Form e o -> Set String
getChangedFields (F model) =
    model.changedFields
