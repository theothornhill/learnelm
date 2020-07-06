module Page.Second exposing (..)

import Html exposing (..)
import Html.Attributes exposing (placeholder, style, value)
import Html.Events exposing (onInput)


type alias Model =
    { text : String }


type Msg
    = ChangeText String


init =
    Model ""


view model =
    { title = "Second"
    , body =
        div []
            [ input
                [ placeholder "Write something here"
                , value model.text
                , onInput ChangeText
                ]
                []
            , div [] [ text model.text ]
            ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeText string ->
            ( { model | text = string }, Cmd.none )
