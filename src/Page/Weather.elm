module Page.Weather exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD


type Model
    = Failure
    | Loading
    | Success String


type Msg
    = GotWeather (Result Http.Error String)
    | GetWeather


type alias Position =
    { latitude : Float
    , longitude : Float
    }

defaultPosition : Position
defaultPosition =
    { latitude = 51.5
    , longitude = 0.0
    }

init : Model
init =
    Success "Get weather by pressing the button"


view : Model -> { title : String, body : Html Msg }
view model =
    { title = "Weather"
    , body =
        div []
            [ viewWeather model ]
    }


weatherView : String -> Html Msg
weatherView t =
    div []
        [ button
            [ onClick GetWeather ]
            [ text "Get Weather" ]
        , text t
        ]


viewWeather : Model -> Html Msg
viewWeather model =
    case model of
        Failure ->
            weatherView "Failed, try again"

        Loading ->
            text "Loading..."

        Success s ->
            weatherView s


update : Msg -> Model -> Position -> ( Model, Cmd Msg )
update msg _ position =
    case msg of
        GotWeather result ->
            case result of
                Ok weather ->
                    ( Success weather, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )

        GetWeather ->
            ( Loading, getWeather position )


getWeather : Position -> Cmd Msg
getWeather position =
    let
        urlWithPosition =
            "https://api.met.no/weatherapi/locationforecast/2.0/compact?lat="
                ++ String.fromFloat position.latitude
                ++ "&lon="
                ++ String.fromFloat position.longitude
    in
    Http.get
        { url = urlWithPosition
        , expect = Http.expectJson GotWeather weatherDecoder
        }


weatherDecoder : JD.Decoder String
weatherDecoder =
    JD.field "geometry" (JD.field "type" JD.string)
