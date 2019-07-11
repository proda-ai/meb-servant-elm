module Main exposing (Model, Msg(..), init, main, tagFromString, update, view, viewOption)

import Date exposing (Date)
import Date.Extra as Date
import DatePicker exposing (DateEvent(..), defaultSettings)
import Generated.GiphyApi as Api
import Html exposing (button, div, img, input, option, p, select, text)
import Html.Attributes exposing (placeholder, selected, src, style, value)
import Html.Events exposing (on, onClick, onInput, targetValue)
import Http
import Json.Decode as Json
import String


main : Program Never Model Msg
main =
    Html.program
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


init : ( Model, Cmd Msg )
init =
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


type Msg
    = FetchGif
    | NewGif (Result Http.Error Api.Gif)
    | ToDatePicker DatePicker.Msg
    | SetTag (Maybe Api.Tags)


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        FetchGif ->
            let
                effects =
                    Api.getRandom (Just "dc6zaTOxFJmzC") model.tag
                        |> Http.send NewGif
            in
            ( { model
                | url = Nothing
              }
            , effects
            )

        NewGif rGif ->
            let
                _ =
                    Debug.log "rx" rGif
            in
            ( { model
                | url =
                    rGif
                        |> Result.toMaybe
                        |> Maybe.map (.data >> .image_url)
                , importDateTime =
                    rGif
                        |> Result.toMaybe
                        |> Maybe.map (.data >> .import_datetime)
              }
            , Cmd.none
            )

        SetTag t ->
            ( { model | tag = t }, Cmd.none )

        ToDatePicker msg ->
            let
                ( newDatePicker, datePickerFx, dateEvent ) =
                    DatePicker.update defaultSettings msg model.datePicker

                newDate =
                    case dateEvent of
                        Changed newDate ->
                            newDate

                        _ ->
                            model.importDateTime
            in
            { model
                | importDateTime = newDate
                , datePicker = newDatePicker
            }
                ! [ Cmd.map ToDatePicker datePickerFx ]


viewOption : Maybe Api.Tags -> Maybe Api.Tags -> Html.Html Msg
viewOption current t =
    option
        [ selected (stringFromTag t == stringFromTag current)
        , value <| stringFromTag t
        ]
        [ text <| prettyStringFromTag t ]


stringFromTag : Maybe Api.Tags -> String
stringFromTag =
    toString


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
                    Date.toFormattedString "ddd MMM y" d

                Nothing ->
                    ""
    in
    div []
        [ div
            [ style
                [ ( "max-width", "38rem" )
                , ( "padding", "2rem" )
                , ( "margin", "auto" )
                , ( "display", "flex" )
                , ( "flex-direction", "column" )
                , ( "font-family", "system-ui, sans-serif" )
                , ( "line-height", "1.6" )
                , ( "color", "#222" )
                ]
            ]
            [ select
                [ on "change" <| Json.map (SetTag << tagFromString) <| targetValue
                , style [ ( "width", "200px" ), ( "height", "2em" ), ( "margin-bottom", "1em" ) ]
                ]
                [ viewOption model.tag (Just Api.Kanye)
                , viewOption model.tag (Just Api.TaylorSwift)
                , viewOption model.tag (Just Api.BruceWayne)
                , viewOption model.tag Nothing
                ]
            , button
                [ onClick FetchGif
                , style [ ( "width", "200px" ), ( "height", "2em" ), ( "margin-bottom", "1em" ) ]
                ]
                [ text "Get Gif" ]
            , p [] [ text <| "Date imported/created: " ++ importDateString ]
            , div
                [ style [ ( "width", "200px" ), ( "height", "2em" ), ( "margin-bottom", "1em" ) ] ]
                [ DatePicker.view model.importDateTime defaultSettings model.datePicker |> Html.map ToDatePicker ]
            , img [ src (Maybe.withDefault "" model.url) ] []
            ]
        ]
