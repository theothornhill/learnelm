module Page exposing (view, Page(..))

import Html exposing (Html)
import Browser

type Page
    = Home
    | Weather
    | WebSocket


view : { title : String, body : Html msg } -> Browser.Document msg
view { title, body } =
    { title = title
    , body = [ body ]
    }
