module Brainfuck.Types exposing (Model, Msg(..), Example, Stdout(..))


type Stdout
    = Empty
    | Success (List Int)
    | Error String


type alias Model =
    { code : String
    , stdin : String
    , stdout : Stdout
    , generation : Int
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
