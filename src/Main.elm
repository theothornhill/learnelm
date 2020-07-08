module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (href, style)
import Page
import Page.Home as Home
import Page.Second as Second
import Page.Weather as Weather
import Url
import Url.Parser as Parser



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , page : SubModel
    }


type SubModel
    = Home Home.Model
    | Weather Weather.Model
    | Second Second.Model


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key
      , url = url
      , page = fromUrl url
      }
    , Cmd.none
    )



-- VIEW


viewLink : String -> String -> Html msg
viewLink path t =
    li
        [ style "list-style" "none"
        , style "justify-self" "center"]
        [ a [ href path ] [ text t ] ]


viewHeader : Model -> Html Msg
viewHeader model =
    let
        url =
            model.url

        homeUrl =
            { url | path = "/home" }

        weatherUrl =
            { url | path = "/weather" }

        secondUrl =
            { url | path = "/second" }
    in
    div []
        [ ul
            [ style "display" "grid"
            , style "grid-template-columns" "1fr 1fr 1fr"
            , style "padding" "unset"
            ]
            [ viewLink (Url.toString homeUrl) "Home"
            , viewLink (Url.toString weatherUrl) "Weather"
            , viewLink (Url.toString secondUrl) "Second"
            ]
        ]


view : Model -> Browser.Document Msg
view model =
    let
        viewPage toMsg details =
            let
                { title, body } =
                    Page.view details

                page =
                    List.map (Html.map toMsg) body
            in
            { title = title
            , body = viewHeader model :: page
            }
    in
    case model.page of
        Home home ->
            viewPage GotHomeMsg (Home.view home)

        Weather weather ->
            viewPage GotWeatherMsg (Weather.view weather)

        Second second ->
            viewPage GotSecondMsg (Second.view second)



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotHomeMsg Home.Msg
    | GotWeatherMsg Weather.Msg
    | GotSecondMsg Second.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case ( message, model.page ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( UrlChanged url, _ ) ->
            ( { model | page = fromUrl url }
            , Cmd.none
            )

        ( GotHomeMsg subMsg, Home home ) ->
            Home.update subMsg home
                |> updateWith Home GotHomeMsg model

        ( GotWeatherMsg subMsg, Weather weather ) ->
            Weather.update subMsg weather Weather.defaultPosition
                |> updateWith Weather GotWeatherMsg model

        ( GotSecondMsg subMsg, Second second ) ->
            Second.update subMsg second
                |> updateWith Second GotSecondMsg model

        _ ->
            ( model, Cmd.none )


updateWith : (subModel -> SubModel) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( { model | page = toModel subModel }
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- URL PARSING


parser : Parser.Parser (SubModel -> a) a
parser =
    Parser.oneOf
        [ Parser.map (Home Home.init) (Parser.s "home")
        , Parser.map (Weather Weather.init) (Parser.s "weather")
        , Parser.map (Second Second.init) (Parser.s "second")
        ]


fromUrl : Url.Url -> SubModel
fromUrl url =
    Maybe.withDefault (Home Home.init) (Parser.parse parser url)
