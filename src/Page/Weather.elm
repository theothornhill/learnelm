module Page.Weather exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Iso8601 as T
import Json.Decode as JD
import Time


type Model
    = Failure
    | Loading
    | Success (List Temperature)


type Msg
    = GotWeather (Result Http.Error (List Temperature))
    | GetWeather


type alias Position =
    { latitude : Float
    , longitude : Float
    }


defaultPosition : Position
defaultPosition =
    { latitude = 60.3
    , longitude = 5.3
    }


init : Model
init =
    Success []


view : Model -> { title : String, body : Html Msg }
view model =
    { title = "Weather"
    , body =
        viewWeather model
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

        Success weather ->
            div []
                [ weatherView "Click on button to get temperatures!"
                , viewTemperatureList weather
                ]


toDayString : Time.Weekday -> String
toDayString day =
    case day of
        Time.Mon ->
            "Mandag"

        Time.Tue ->
            "Tirsdag"

        Time.Wed ->
            "Onsdag"

        Time.Thu ->
            "Torsdag"

        Time.Fri ->
            "Fredag"

        Time.Sat ->
            "Lørdag"

        Time.Sun ->
            "Søndag"


type alias TempView =
    { day : String
    , hour : String
    , temp : String
    }


viewTemperatureList : List Temperature -> Html Msg
viewTemperatureList temperatures =
    let
        weekDayValue day =
            Time.toWeekday Time.utc day

        days temps =
            List.map
                (\day ->
                    TempView
                        (toDayString (weekDayValue (.time day)))
                        (String.fromInt (Time.toHour Time.utc (.time day)))
                        (String.fromFloat (.temp day))
                )
                temps
    in
    div []
        [ ul []
            (List.map
                (\d ->
                    li []
                        [ text (.day d ++ " kl " ++ .hour d ++ ": " ++ .temp d ++ " grader") ]
                )
                (days temperatures)
            )
        ]


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


type alias Temperature =
    { time : Time.Posix
    , temp : Float
    }


weatherDecoder : JD.Decoder (List Temperature)
weatherDecoder =
    temperatureEveryHour


temperatureEveryHour : JD.Decoder (List Temperature)
temperatureEveryHour =
    JD.list
        (JD.map2 Temperature
            (JD.field "time" T.decoder)
            temperatureDecoder
        )
        |> JD.field "timeseries"
        |> JD.field "properties"


temperatureDecoder : JD.Decoder Float
temperatureDecoder =
    JD.at [ "data", "instant", "details", "air_temperature" ] JD.float
