module Main exposing (..)

import Generated.GiphyApi as Api
import Html exposing (div, img, input, button, text)
import Html.Attributes exposing (placeholder, src, value)
import Html.Events exposing (onClick, onInput, targetValue)
import Http
import String
import Browser


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { url : Maybe String
    , topic : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { url = Nothing
      , topic = Nothing
      }
    , Cmd.none
    )


type Msg
    = FetchGif
    | NewGif (Result (Maybe (Http.Metadata, String), Http.Error) Api.Gif)
    | SetTopic String


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        FetchGif ->
            let
                effects =
                    Api.getRandom NewGif (Just "dc6zaTOxFJmzC") model.topic
            in
                ( { model
                    | url = Nothing
                  }
                , effects
                )

        NewGif (Err e) ->
            let
                _ = Debug.log "err" <| Debug.toString e
            in
                ( model, Cmd.none )


        NewGif (Ok rGif) ->
            ( { model
                | url = rGif |> .data |> .image_url |> Just
              }
            , Cmd.none
            )

        SetTopic topic ->
            ( { model
                | topic =
                    if String.isEmpty topic then
                        Nothing
                    else
                        Just topic
              }
            , Cmd.none
            )


view : Model -> Html.Html Msg
view model =
    div []
        [ div []
            [ input
                [ onInput SetTopic
                , value (Maybe.withDefault "" model.topic)
                , placeholder "topic"
                ]
                []
            , button
                [ onClick FetchGif ]
                [ text "click me" ]
            ]
        , img [ src (Maybe.withDefault "" model.url) ] []
        ]
