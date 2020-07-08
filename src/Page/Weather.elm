module Page.Weather exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Http
import Iso8601 as T
import Json.Decode as JD
import Time


type Model
    = Failure String
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


getWeatherButton : String -> Html Msg
getWeatherButton t =
    div [ style "justify-self" "center" ]
        [ button
            [ onClick GetWeather ]
            [ text "Get Weather" ]
        , div [] [ text t ]
        ]


viewWeather : Model -> Html Msg
viewWeather model =
    case model of
        Failure str ->
            getWeatherButton str

        Loading ->
            text "Loading..."

        Success weather ->
            div [ style "display" "grid" ]
                [ getWeatherButton ""
                , viewCurrentTemperature weather
                , viewTemperatureList weather
                ]


viewCurrentTemperature : List Temperature -> Html Msg
viewCurrentTemperature weather =
    let
        head =
            List.head weather

        headVal =
            case head of
                Just val ->
                    "It is now " ++ String.fromFloat (.temp val) ++ " degrees"

                Nothing ->
                    ""
    in
    div
        [ style "justify-self" "end"
        , style "padding-right" "5em"
        , style "font-size" "16pt"
        ]
        [ text headVal ]


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
    , hour : Int
    , temp : String
    }


createTempViews : List Temperature -> List TempView
createTempViews temps =
    let
        weekDayValue day =
            Time.toWeekday Time.utc day
    in
    List.map
        (\day ->
            TempView
                (toDayString (weekDayValue (.time day)))
                (Time.toHour Time.utc (.time day))
                (String.fromFloat (.temp day))
        )
        temps


viewTemperature : TempView -> Html Msg
viewTemperature temp =
    let
        hourString =
            String.fromInt (.hour temp) ++ ":00"

        time =
            if .hour temp < 10 then
                "0" ++ hourString

            else
                hourString
    in
    li [ style "border-top" "1px solid black" ]
        [ div
            [ style "display" "grid"
            , style "grid-template-columns" "1fr 1fr 1fr"
            , style "min-height" "5em"
            ]
            [ div
                [ style "justify-self" "center"
                , style "align-self" "center"
                ]
                [ text (.day temp) ]
            , div
                [ style "justify-self" "center"
                , style "align-self" "center"
                ]
                [ text time ]
            , div
                [ style "justify-self" "center"
                , style "align-self" "center"
                ]
                [ text (.temp temp ++ " grader") ]
            ]
        ]


viewTemperatureList : List Temperature -> Html Msg
viewTemperatureList temperatures =
    div []
        [ ul
            [ style "padding" "unset" ]
            (List.map (\d -> viewTemperature d) (createTempViews temperatures))
        ]


update : Msg -> Model -> Position -> ( Model, Cmd Msg )
update msg _ position =
    case msg of
        GotWeather result ->
            case result of
                Ok weather ->
                    ( Success weather, Cmd.none )

                Err err ->
                    case err of
                        Http.BadUrl str ->
                            ( Failure str, Cmd.none )

                        Http.Timeout ->
                            ( Failure "Timeout", Cmd.none )

                        Http.NetworkError ->
                            ( Failure "NetworkError", Cmd.none )

                        Http.BadStatus int ->
                            ( Failure (String.fromInt int), Cmd.none )

                        Http.BadBody str ->
                            ( Failure str, Cmd.none )

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
