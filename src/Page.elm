module Page exposing (..)

import Html exposing (..)
import Browser

type Page
    = Home
    | Weather
    | Second


view : { title : String, body : Html msg } -> Browser.Document msg
view { title, body } =
    { title = title
    , body = [ body ]
    }
