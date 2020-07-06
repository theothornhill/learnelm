module Page exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href, style)
import Url
import Browser

type Page
    = Home
    | First
    | Second


view : Page -> { title : String, body : Html msg } -> Browser.Document msg
view page { title, body } =
    { title = title
    , body = body :: []
    }
