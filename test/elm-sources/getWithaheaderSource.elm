module GetWithAHeaderSource exposing (..)

import Http
import String.Conversions as String
import Json.Decode exposing (..)


getWithaheader : String -> Int -> Task.Task (Maybe (Http.Metadata, String), Http.Error) (String)
getWithaheader header_myStringHeader header_MyIntHeader =
    Http.task
        { method =
            "GET"
        , headers =
            [ Http.header "myStringHeader" (header_myStringHeader)
            , Http.header "MyIntHeader" (String.fromInt header_MyIntHeader)
            ]
        , url =
            String.join "/"
                [ ""
                , "with-a-header"
                ]
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
                            (decodeString string body_)
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody
                                |> Result.mapError (Tuple.pair (Just (metadata, body_))))
        , timeout =
            Nothing
        }
