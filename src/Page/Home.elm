module Page.Home exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)


type alias Model =
    { text : String }


type Msg
    = ChangeText String


init : Model
init =
    Model "Click a link to navigate"


view : Model -> { title : String, body : Html Msg }
view model =
    { title = "Home"
    , body =
        div
            [ style "display" "grid"
            , style "justify-items" "center"
            ]
            [ text model.text ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeText string ->
            ( { model | text = string }, Cmd.none )
