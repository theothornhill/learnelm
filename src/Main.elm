module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Page
import Page.First as First
import Page.Home as Home
import Page.Second as Second
import Url
import Url.Parser as Parser
import Html.Attributes exposing (href)



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
    | First First.Model
    | Second Second.Model


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotHomeMsg Home.Msg
    | GotFirstMsg First.Msg
    | GotSecondMsg Second.Msg


type alias Details msg =
    { title : String
    , body : List (Html msg)
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key
      , url = url
      , page = fromUrl url
      }
    , Cmd.none
    )


viewLink : String -> String -> Html msg
viewLink path t =
    li
        []
        [ a [ href path ] [ text t ] ]


viewHeader model =
    let
        url =
            model.url

        homeUrl =
            { url | path = "/home" }

        firstUrl =
            { url | path = "/first" }

        secondUrl =
            { url | path = "/second" }
    in
    div []
        [ ul
            []
            [ viewLink (Url.toString homeUrl) "Home"
            , viewLink (Url.toString firstUrl) "First"
            , viewLink (Url.toString secondUrl) "Second"
            ]
        ]


view model =
    let
        viewPage page toMsg details =
            let
                { title, body } =
                    Page.view page details
            in
            { title = title
            , body = viewHeader model :: (List.map (Html.map toMsg) body)
            }
    in
    case model.page of
        Home home ->
            viewPage Page.Home GotHomeMsg (Home.view home)

        First first ->
            viewPage Page.First GotFirstMsg (First.view first)

        Second second ->
            viewPage Page.Second GotSecondMsg (Second.view second)


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

        ( GotFirstMsg subMsg, First first ) ->
            First.update subMsg first
                |> updateWith First GotFirstMsg model

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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


parser : Parser.Parser (SubModel -> a) a
parser =
    Parser.oneOf
        [ Parser.map (Home Home.init) (Parser.s "home")
        , Parser.map (First First.init) (Parser.s "first")
        , Parser.map (Second Second.init) (Parser.s "second")
        ]


fromUrl : Url.Url -> SubModel
fromUrl url =
    Maybe.withDefault (First First.init) (Parser.parse parser url)
