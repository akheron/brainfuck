module Types exposing (Model, Msg(..), Example)

import Html exposing (Html)


type alias Model =
    { code : String
    , stdin : String
    , stdout : List (Html Msg)
    }


type Msg
    = Code String
    | Stdin String
    | ShowExample Example
    | Run


type alias Example =
    { name : String
    , code : String
    , stdin : String
    }
