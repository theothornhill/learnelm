port module Page.WebSocket exposing (..)

import Html exposing (..)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as JD


type alias Model =
    { draft : String
    , messages : List String
    }



-- PORTS


port sendMessage : String -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg


type Msg
    = ChangeText String
    | Send
    | Receive String


init : Model
init =
    Model "" []



-- VIEW


view model =
    { title = "Ports"
    , body =
        div []
            [ h1 [] [ text "Echo Chat" ]
            , ul []
                (List.map (\msg -> li [] [ text msg ]) model.messages)
            , input
                [ type_ "text"
                , placeholder "Draft"
                , onInput ChangeText
                , on "keydown" (ifIsEnter Send)
                , value model.draft
                ]
                []
            , button [ onClick Send ] [ text "Send" ]
            ]
    }



-- DETECT ENTER


ifIsEnter : msg -> JD.Decoder msg
ifIsEnter msg =
    JD.field "key" JD.string
        |> JD.andThen
            (\key ->
                if key == "Enter" then
                    JD.succeed msg

                else
                    JD.fail "some other key"
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeText string ->
            ( { model | draft = string }, Cmd.none )

        Send ->
            ( { model | draft = "" }, sendMessage model.draft )

        Receive message ->
            ( { model | messages = model.messages ++ [ message ] }, Cmd.none )
