module Model exposing (..)

import Form exposing (Form)
import Form.Field as Field
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


initialFields : List ( String, Field.Field )
initialFields =
    [ ( "name", Field.Text "hey" )
    , ( "profile"
      , Field.group
            [ ( "foo", Field.Radio "ho" ) ]
      )
    ]


roles : List String
roles =
    [ "role1", "role2" ]


superpowers : List String
superpowers =
    [ "flying", "invisible" ]


{-| Infix operators. See elm-simple-form-infix for a packaged version.
-}
(:=) =
    Validate.get
infixl 7 :=


(|:) =
    Validate.apply


validate : Validation CustomError User
validate =
    form4
        User
        ("name" := string `andThen` nonEmpty)
        ("email" := email `andThen` (asyncCheck True))
        ("admin" := bool |> defaultValue False)
        ("profile" := validateProfile)


validateProfile : Validation CustomError Profile
validateProfile =
    succeed Profile
        |: ("website"
                := oneOf
                    [ emptyString |> map (\_ -> Nothing)
                    , url |> map Just
                    ]
           )
        |: ("role" := (string `andThen` (includedIn roles)))
        |: ("superpower" := validateSuperpower)
        |: ("age" := naturalInt)
        |: ("bio" := string |> defaultValue "")


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
