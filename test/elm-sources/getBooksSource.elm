module GetBooksSource exposing (..)

import Http
import String.Conversions as String
import Json.Decode exposing (..)
import Url


getBooks : Bool -> Maybe (String) -> Maybe (Int) -> List (Maybe (Bool)) -> Task.Task (Maybe (Http.Metadata, String), Http.Error) (List (Book))
getBooks query_published query_sort query_year query_filters =
    let
        params =
            List.filter (not << String.isEmpty)
                [ if query_published then
                    "query_published="
                  else
                    ""
                , query_sort
                    |> Maybe.map ( Url.percentEncode >> (++) "sort=")
                    |> Maybe.withDefault ""
                , query_year
                    |> Maybe.map (String.fromInt >> Url.percentEncode >> (++) "year=")
                    |> Maybe.withDefault ""
                , query_filters
                    |> List.map (\val -> "query_filters[]=" ++ (val |> Maybe.map (String.fromBool) |> Maybe.withDefault "" |> Url.percentEncode))
                    |> String.join "&"
                ]
    in
        Http.task
            { method =
                "GET"
            , headers =
                []
            , url =
                String.join "/"
                    [ ""
                    , "books"
                    ]
                ++ if List.isEmpty params then
                       ""
                   else
                       "?" ++ String.join "&" params
            , body =
                Http.emptyBody
            , resolver =
                Http.stringResolver
                    (\res ->
                        case res of
                            Http.BadUrl_ url -> Err (Nothing, Http.BadUrl url)
                            Http.Timeout_ -> Err (Nothing, Http.Timeout)
                            Http.NetworkError_ -> Err (Nothing, Http.NetworkError)
                            Http.BadStatus_ metadata body_ -> Err (Just (metadata, body_), Http.BadStatus metadata.statusCode)
                            Http.GoodStatus_ metadata body_ ->
                                (decodeString (list decodeBook) body_)
                                    |> Result.mapError Json.Decode.errorToString
                                    |> Result.mapError Http.BadBody
                                    |> Result.mapError (Tuple.pair (Just (metadata, body_))))
            , timeout =
                Nothing
            }
