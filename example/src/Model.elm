module Model where

import String

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as Validate exposing (..)


type Action =
  NoOp | FormAction Form.Action | SubmitUser User


type alias Model =
  { form : Form CustomError User
  , userMaybe : Maybe User
  }

type CustomError = Ooops | Nope | AlreadyTaken


type alias User =
  { name : String
  , email : String
  , age : Int
  , admin : Bool
  , role : Maybe String
  , profile : Profile
  }


type alias Profile =
  { foo : String
  , bar : String
  }


initialFields : List (String, Field.Field)
initialFields =
  [ ("name", Field.text "hey")
  , ("profile", Field.group
       [ ("foo", Field.radio "ho") ]
    )
  ]


foos : List String
foos =
  [ "hey", "ho" ]


validate : Validation CustomError User
validate =
  form6 User
    ("name" := (string |> map String.trim) `andThen` nonEmpty)
    ("email" := string `andThen` email)
    ("age" := naturalInt)
    ("admin" := bool |> defaultValue False)
    ("role" ?= string)
    ("profile" := form2 Profile
      ("foo" := string `andThen` (includedIn foos))
      ("bar" := string `andThen` (asyncCheck True)))


customString : List String -> Validation CustomError String
customString options =
  customValidation string (\s -> Ok s)


-- eq. to: int `andThen` (minInt 0)
naturalInt : Validation CustomError Int
naturalInt =
  customValidation int (\i -> if i > 0 then Ok i else Err (customError Nope))


asyncCheck : Bool -> String -> Validation CustomError String
asyncCheck serverIsOk s =
  if serverIsOk
    then succeed s
    else fail (customError AlreadyTaken)

