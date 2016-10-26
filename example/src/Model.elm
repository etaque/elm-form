module Model exposing (..)

import Form exposing (Form)
import Form.Field as Field exposing (Field, field, group)
import Form.Validate as Validate exposing (..)


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


type alias User =
    { name : String
    , email : String
    , admin : Bool
    , profile : Profile
    , links : List Link
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


type alias Link =
    { name : String
    , url : String
    }


initialFields : List ( String, Field )
initialFields =
    [ field "name" (Field.Text "hey")
    , group "profile"
        [ field "age" (Field.Text "33") ]
    ]


roles : List String
roles =
    [ "role1", "role2" ]


superpowers : List String
superpowers =
    [ "flying", "invisible" ]


validate : Validation CustomError User
validate =
    form5
        User
        (get "name" (string `andThen` nonEmpty))
        (get "email" (email `andThen` (asyncCheck True)))
        (get "admin" (bool |> defaultValue False))
        (get "profile" validateProfile)
        (get "links" (list validateLink))


validateProfile : Validation CustomError Profile
validateProfile =
    succeed Profile
        `apply`
            (get "website"
                (oneOf
                    [ emptyString |> map (\_ -> Nothing)
                    , url |> map Just
                    ]
                )
            )
        `apply` (get "role" (string `andThen` (includedIn roles)))
        `apply` (get "superpower" validateSuperpower)
        `apply` (get "age" naturalInt)
        `apply` (get "bio" (string |> defaultValue ""))


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


validateLink : Validation CustomError Link
validateLink =
    form2 Link
        (get "name" (string `andThen` (minLength 3)))
        (get "url" url)



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
