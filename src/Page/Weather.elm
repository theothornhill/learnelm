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


init =
    Success "Get weather by pressing the button"


view : Model -> { title : String, body : Html Msg }
view model =
    { title = "Weather"
    , body =
        div []
            [ viewWeather model ]
    }


viewWeather : Model -> Html Msg
viewWeather model =
    case model of
        Failure ->
            div []
                [ button
                    [ onClick GetWeather ]
                    [ text "Get Weather" ]
                , text "Failed, try again"
                ]

        Loading ->
            text "Loading..."

        Success s ->
            div []
                [ button
                    [ onClick GetWeather ]
                    [ text "Get Weather" ]
                , text s
                ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotWeather result ->
            case result of
                Ok weather ->
                    ( Success weather, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )

        GetWeather ->
            ( Loading, getWeather )


getWeather : Cmd Msg
getWeather =
    Http.get
        { url = "https://api.met.no/weatherapi/locationforecast/2.0/compact?lat=51.5&lon=0"
        , expect = Http.expectJson GotWeather weatherDecoder
        }


weatherDecoder : JD.Decoder String
weatherDecoder =
    JD.field "geometry" (JD.field "type" JD.string)
