module Form exposing
    ( Msg(..), InputType(..), Form, FieldState
    , initial, update
    , getFieldAsString, getFieldAsBool, getListIndexes
    , getFocus, isSubmitted, getErrors, getOutput, getChangedFields
    )

{-| Simple forms made easy: A Dict implementation of the core `Json.Decode` API,
with state lifecycle and input helpers for the views.


# Types

@docs Msg, InputType, Form, FieldState


# Init/update lifecyle

@docs initial, update


# Field state accessors

@docs getFieldAsString, getFieldAsBool, getListIndexes


# Global state accessors

@docs getFocus, isSubmitted, getErrors, getOutput, getChangedFields

-}

import Dict exposing (Dict)
import Form.Error as Error exposing (Error, ErrorValue)
import Form.Field as Field exposing (Field, FieldValue)
import Form.Tree as Tree
import Form.Validate as Validate exposing (Validation)
import Result
import Set exposing (Set)


{-| Form to embed in your model. Type parameters are:

  - `customError` - a custom error type to extend built-in errors (set to `()` if you don't need it)
  - `output` - the type of the validation output.

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
    , originalValues : Dict String (Maybe FieldValue)
    , isSubmitted : Bool
    , output : Maybe output
    , errors : Error customError
    }


{-| Initial form state. See `Form.Field` for initial fields, and `Form.Validate` for validation.
-}
initial : List ( String, Field ) -> Validation e output -> Form e output
initial initialFields validation =
    let
        model =
            { fields = Tree.group initialFields
            , focus = Nothing
            , dirtyFields = Set.empty
            , changedFields = Set.empty
            , originalValues = Dict.empty
            , isSubmitted = False
            , output = Nothing
            , errors = Tree.group []
            }
    in
    F (updateValidate validation model)


{-| Field state containing all necessary data for view and update,
can be retrived with `Form.getFieldAsString` or `Form.getFieldAsBool`.

  - `path` - qualified path of the field in the form, with dots for nested fields (`field.subfield`)
  - `value` - a `Maybe` of the requested type
  - `error` - a `Maybe` of the field error
  - `liveError` - same but with added logic for live validation
    (see [`getLiveErrorAt`](https://github.com/etaque/elm-form/blob/master/src/Form.elm) impl)
  - `isDirty` - if the field content has been changed since last validation
  - `isChanged` - if the field value has changed since last init/reset
  - `hasFocus` - if the field is currently focused

-}
type alias FieldState e a =
    { path : String
    , value : Maybe a
    , error : Maybe (ErrorValue e)
    , liveError : Maybe (ErrorValue e)
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


{-| return a list of indexes so one can build qualified names of fields in list.
-}
getListIndexes : String -> Form e o -> List Int
getListIndexes path (F model) =
    let
        length =
            getFieldAt path model
                |> Maybe.map (Tree.asList >> List.length)
                |> Maybe.withDefault 0
    in
    List.range 0 (length - 1)


{-| Form messages for `update`.
-}
type Msg
    = NoOp
    | Focus String
    | Blur String
    | Input String InputType FieldValue
    | Append String
    | RemoveItem String Int
    | Submit
    | Validate
    | Reset (List ( String, Field ))


{-| Input types to determine live validation behaviour.
-}
type InputType
    = Text
    | Textarea
    | Select
    | Radio
    | Checkbox


{-| Update form state with the given message
-}
update : Validation e output -> Msg -> Form e output -> Form e output
update validation msg (F model) =
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
            F (updateValidate validation newModel)

        Input name inputType fieldValue ->
            let
                newFields =
                    setFieldAt name (Tree.Value fieldValue) model

                isDirty =
                    case inputType of
                        Text ->
                            True

                        Textarea ->
                            True

                        _ ->
                            False

                newDirtyFields =
                    if isDirty then
                        Set.insert name model.dirtyFields

                    else
                        model.dirtyFields

                ( newChangedFields, newOriginalValues ) =
                    if Set.member name model.changedFields then
                        let
                            storedValue =
                                Dict.get name model.originalValues
                                    |> Maybe.withDefault Nothing

                            shouldBeNothing v =
                                case v of
                                    Field.String "" ->
                                        True

                                    Field.Bool False ->
                                        True

                                    _ ->
                                        False

                            sameAsOriginal =
                                case storedValue of
                                    Just v ->
                                        v == fieldValue

                                    Nothing ->
                                        shouldBeNothing fieldValue

                            changedFields =
                                if sameAsOriginal then
                                    Set.remove name model.changedFields

                                else
                                    model.changedFields
                        in
                        ( changedFields, model.originalValues )

                    else
                        let
                            originalValue =
                                getFieldAt name model |> Maybe.andThen Tree.asValue
                        in
                        ( Set.insert name model.changedFields, Dict.insert name originalValue model.originalValues )

                newModel =
                    { model
                        | fields = newFields
                        , dirtyFields = newDirtyFields
                        , changedFields = newChangedFields
                        , originalValues = newOriginalValues
                    }
            in
            F (updateValidate validation newModel)

        Append listName ->
            let
                listFields =
                    getFieldAt listName model
                        |> Maybe.map Tree.asList
                        |> Maybe.withDefault []

                newListFields =
                    listFields ++ [ Tree.Value Field.EmptyField ]

                newModel =
                    { model
                        | fields = setFieldAt listName (Tree.List newListFields) model
                    }
            in
            F newModel

        RemoveItem listName index ->
            let
                listFields =
                    getFieldAt listName model
                        |> Maybe.map Tree.asList
                        |> Maybe.withDefault []

                fieldNamePattern =
                    listName ++ String.fromInt index

                filterChangedFields =
                    Set.filter (not << String.startsWith fieldNamePattern)

                filterOriginalValue =
                    Dict.filter (\c _ -> not <| String.startsWith fieldNamePattern c)

                newListFields =
                    List.take index listFields ++ List.drop (index + 1) listFields

                newModel =
                    { model
                        | fields = setFieldAt listName (Tree.List newListFields) model
                        , changedFields = filterChangedFields model.changedFields
                        , originalValues = filterOriginalValue model.originalValues
                    }
            in
            F (updateValidate validation newModel)

        Submit ->
            let
                validatedModel =
                    updateValidate validation model
            in
            F { validatedModel | isSubmitted = True }

        Validate ->
            F (updateValidate validation model)

        Reset fields ->
            let
                newModel =
                    { model
                        | fields = Tree.group fields
                        , dirtyFields = Set.empty
                        , changedFields = Set.empty
                        , originalValues = Dict.empty
                        , isSubmitted = False
                    }
            in
            F (updateValidate validation newModel)


updateValidate : Validation e o -> Model e o -> Model e o
updateValidate validation model =
    case validation model.fields of
        Ok output ->
            { model
                | errors =
                    Tree.group []
                , output = Just output
            }

        Err error ->
            { model
                | errors =
                    error
                , output = Nothing
            }


getFieldAt : String -> Model e o -> Maybe Field
getFieldAt qualifiedName model =
    Tree.getAtPath qualifiedName model.fields


getStringAt : String -> Form e o -> Maybe String
getStringAt name (F model) =
    getFieldAt name model |> Maybe.andThen Field.asString


getBoolAt : String -> Form e o -> Maybe Bool
getBoolAt name (F model) =
    getFieldAt name model |> Maybe.andThen Field.asBool


setFieldAt : String -> Field -> Model e o -> Field
setFieldAt path field model =
    Tree.setAtPath path field model.fields


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
getErrors : Form e o -> List ( String, Error.ErrorValue e )
getErrors (F model) =
    Tree.valuesWithPath model.errors


getErrorAt : String -> Form e o -> Maybe (ErrorValue e)
getErrorAt path (F model) =
    Tree.getAtPath path model.errors |> Maybe.andThen Tree.asValue


getLiveErrorAt : String -> Form e o -> Maybe (ErrorValue e)
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


{-| Get set of changed fields.
-}
getChangedFields : Form e o -> Set String
getChangedFields (F model) =
    model.changedFields
