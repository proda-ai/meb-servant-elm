module Main exposing (Model, Msg(..), init, main, tagFromString, update, view, viewOption)

import Browser
import Date exposing (Date)
import DatePicker exposing (DateEvent(..), defaultSettings)
import Generated.GiphyApi as Api
import Html exposing (button, div, img, input, option, p, select, text)
import Html.Attributes exposing (placeholder, selected, src, style, value)
import Html.Events exposing (on, onClick, onInput, targetValue)
import Http
import Json.Decode as Json
import String
import Task


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { url : Maybe String
    , importDateTime : Maybe Date
    , datePicker : DatePicker.DatePicker
    , tag : Maybe Api.Tags
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( datePicker, datePickerFx ) =
            DatePicker.init
    in
    ( { url = Nothing
      , importDateTime = Nothing
      , datePicker = datePicker
      , tag = Nothing
      }
    , Cmd.map ToDatePicker datePickerFx
    )


printError : ( Maybe ( Http.Metadata, String ), Http.Error ) -> Int
printError me =
    case me of
        ( Just ( meta, s ), e ) ->
            let
                _ =
                    Debug.log "rec err" e
            in
            1

        ( Nothing, e ) ->
            let
                _ =
                    Debug.log "rec err" e
            in
            1


type Msg
    = FetchGif
    | NewGif (Result ( Maybe ( Http.Metadata, String ), Http.Error ) Api.Gif)
    | ToDatePicker DatePicker.Msg
    | SetTag (Maybe Api.Tags)


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        FetchGif ->
            let
                effects =
                    Task.attempt NewGif <| Api.getRandom (Just "dc6zaTOxFJmzC") model.tag
            in
            ( { model
                | url = Nothing
              }
            , effects
            )

        NewGif (Err e) ->
            let
                _ =
                    printError e
            in
            ( model, Cmd.none )

        NewGif (Ok rGif) ->
            ( { model
                | url = rGif |> .data |> .image_url |> Just
                , importDateTime = rGif |> .data |> .import_datetime |> Just
              }
            , Cmd.none
            )

        SetTag t ->
            let
                _ =
                    Debug.log "yep" t
            in
            ( { model | tag = t }, Cmd.none )

        ToDatePicker msg ->
            let
                ( newDatePicker, dateEvent ) =
                    DatePicker.update defaultSettings msg model.datePicker

                newDate =
                    case dateEvent of
                        Picked d ->
                            Just d

                        _ ->
                            model.importDateTime
            in
            ( { model
                | importDateTime = newDate
                , datePicker = newDatePicker
              }
            , Cmd.none
            )


viewOption : Maybe Api.Tags -> Maybe Api.Tags -> Html.Html Msg
viewOption current t =
    option
        [ selected (stringFromTag t == stringFromTag current)
        , value <| stringFromTag t
        ]
        [ text <| prettyStringFromTag t ]


stringFromTag : Maybe Api.Tags -> String
stringFromTag mt =
    case mt of
        Just Api.Kanye ->
            "Just Kanye"

        Just Api.TaylorSwift ->
            "Just TaylorSwift"

        Just Api.BruceWayne ->
            "Just BruceWayne"

        Nothing ->
            "Nothing"


prettyStringFromTag : Maybe Api.Tags -> String
prettyStringFromTag mt =
    case mt of
        Just Api.Kanye ->
            "Kanye West"

        Just Api.TaylorSwift ->
            "Taylor Swift"

        Just Api.BruceWayne ->
            "Bruce Wayne"

        Nothing ->
            "No Person"


tagFromString : String -> Maybe Api.Tags
tagFromString t =
    case t of
        "Just Kanye" ->
            Just Api.Kanye

        "Just TaylorSwift" ->
            Just Api.TaylorSwift

        "Just BruceWayne" ->
            Just Api.BruceWayne

        _ ->
            Nothing


view : Model -> Html.Html Msg
view model =
    let
        importDateString =
            case model.importDateTime of
                Just d ->
                    Date.format "ddd MMM y" d

                Nothing ->
                    ""
    in
    div []
        [ div
            [ style "max-width" "38rem"
            , style "padding" "2rem"
            , style "margin" "auto"
            , style "display" "flex"
            , style "flex-direction" "column"
            , style "font-family" "system-ui, sans-serif"
            , style "line-height" "1.6"
            , style "color" "#222"
            ]
            [ select
                [ on "change" <| Json.map (SetTag << tagFromString) <| targetValue
                , style "width" "200px"
                , style "height" "2em"
                , style "margin-bottom" "1em"
                ]
                [ viewOption model.tag (Just Api.Kanye)
                , viewOption model.tag (Just Api.TaylorSwift)
                , viewOption model.tag (Just Api.BruceWayne)
                , viewOption model.tag Nothing
                ]
            , button
                [ onClick FetchGif
                , style "width" "200px"
                , style "height" "2em"
                , style "margin-bottom" "1em"
                ]
                [ text "Get Gif" ]
            , p [] [ text <| "Date imported/created: " ++ importDateString ]
            , div
                [ style "width" "200px", style "height" "2em", style "margin-bottom" "1em" ]
                [ DatePicker.view model.importDateTime defaultSettings model.datePicker |> Html.map ToDatePicker ]
            , img [ src (Maybe.withDefault "" model.url) ] []
            ]
        ]
