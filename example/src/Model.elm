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

type CustomError = Ooops


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


validate : Validate CustomError User
validate =
  form6 User
    ("name" := (string |> map String.trim) `andThen` nonEmpty)
    ("email" := string `andThen` email)
    ("age" := int `andThen` (minInt 18) |> customError Ooops)
    ("admin" := bool |> defaultValue False)
    ("role" ?= string)
    ("profile" := form2 Profile
      ("foo" := string `andThen` (includedIn foos))
      ("bar" := string))



