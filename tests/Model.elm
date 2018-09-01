module Model exposing (CustomError(..), Date, Model, Msg(..), Profile, Superpower(..), Todo, User, asyncCheck, dateParser, initialFields, naturalInt, roles, superpowers, validate, validateDate, validateProfile, validateSuperpower, validateTodo, validateUrl)

import Form exposing (Form)
import Form.Error as Error
import Form.Field as Field exposing (Field)
import Form.Validate as Validate exposing (..)
import Parser exposing ((|.), (|=), Parser)
import Regex


type Msg
    = NoOp
    | FormMsg Form.Msg


type alias Model =
    { form : Form CustomError User
    , userMaybe : Maybe User
    }


type CustomError
    = Ooops
    | Nope
    | AlreadyTaken
    | InvalidSuperpower
    | InvalidDate


type alias Date =
    { year : Int
    , month : Int
    , day : Int
    }


type alias User =
    { name : String
    , email : String
    , admin : Bool
    , birthday : Date
    , profile : Profile
    , todos : List Todo
    }


type alias Profile =
    { website : Maybe String
    , role : String
    , superpower : Superpower
    , age : Int
    , bio : String
    }


type Superpower
    = Flying
    | Invisible


type alias Todo =
    { done : Bool
    , label : String
    }


initialFields : List ( String, Field )
initialFields =
    [ ( "name", Field.string "hey" )
    , ( "profile"
      , Field.group
            [ ( "age", Field.string "33" ) ]
      )
    , ( "todos"
      , Field.list
            [ Field.group
                [ ( "done", Field.bool True )
                , ( "label", Field.string "Remember the milk" )
                ]
            ]
      )
    ]


roles : List String
roles =
    [ "role1", "role2" ]


superpowers : List String
superpowers =
    [ "flying", "invisible" ]


validate : Validation CustomError User
validate =
    map6
        User
        (field "name" (string |> andThen nonEmpty))
        (field "email" (email |> andThen (asyncCheck True)))
        (field "admin" (bool |> defaultValue False))
        (field "date" validateDate)
        (field "profile" validateProfile)
        (field "todos" (list validateTodo))


validateDate : Validation CustomError Date
validateDate =
    let
        parseDate text =
            text
                |> Parser.run dateParser
                |> Result.mapError (always (Error.value (Error.CustomError InvalidDate)))

        -- This should use much more complicated logic to ensure it's actually valid
        validateDayIsValid date validationField =
            if date.month > 12 || date.month < 1 then
                Err (Error.value (Error.CustomError InvalidDate))

            else if date.day > 31 || date.day < 1 then
                Err (Error.value (Error.CustomError InvalidDate))

            else
                Ok date
    in
    customValidation string parseDate
        |> andThen validateDayIsValid
        |> mapError (always (Error.value (Error.CustomError InvalidDate)))


dateParser : Parser Date
dateParser =
    Parser.succeed Date
        |= Parser.int
        |. Parser.symbol "-"
        |. Parser.chompIf ((==) '0')
        |= Parser.int
        |. Parser.symbol "-"
        |. Parser.chompIf ((==) '0')
        |= Parser.int
        |. Parser.end


validateProfile : Validation CustomError Profile
validateProfile =
    succeed Profile
        |> andMap
            (field "website"
                (oneOf
                    [ emptyString |> map (\_ -> Nothing)
                    , validateUrl |> map Just
                    ]
                )
            )
        |> andMap (field "role" (string |> andThen (includedIn roles)))
        |> andMap (field "superpower" validateSuperpower)
        |> andMap (field "age" naturalInt)
        |> andMap (field "bio" (string |> defaultValue ""))


validateSuperpower : Validation CustomError Superpower
validateSuperpower =
    customValidation
        string
        (\s ->
            case s of
                "flying" ->
                    Ok Flying

                "invisible" ->
                    Ok Invisible

                _ ->
                    Err (customError InvalidSuperpower)
        )


validateTodo : Validation CustomError Todo
validateTodo =
    map2 Todo
        (field "done" bool)
        (field "label" string)


{-| Check if the string is a valid URL.
-}
validateUrl : Validation e String
validateUrl =
    let
        urlRegex =
            Regex.fromString "^(https?://)"
                |> Maybe.withDefault Regex.never
    in
    string
        |> andThen (format urlRegex)



-- eq. to: int `andThen` (minInt 0)


naturalInt : Validation CustomError Int
naturalInt =
    customValidation
        int
        (\i ->
            if i > 0 then
                Ok i

            else
                Err (customError Nope)
        )


asyncCheck : Bool -> String -> Validation CustomError String
asyncCheck serverIsOk s =
    if serverIsOk then
        succeed s

    else
        fail (customError AlreadyTaken)
