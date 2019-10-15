module Monitor exposing (HeaderEntry, CommunicateEvent(..), RequestData, ResponseData, decodeRequestResponse)

import Json.Decode as Decode exposing (Decoder, andThen, decodeString, fail, field, int, list, map, map2, map8, nullable, string)
import Time exposing (Posix, millisToPosix)


type alias HeaderEntry =
    { name: String
    , value: String
    }


type alias RequestData =
    { id: String
    , httpVersion: String
    , headers: List HeaderEntry
    , url: String
    , method: String
    , body: Maybe String
    , start: Posix
    , end: Posix
    }


type alias ResponseData =
    { id: String
    , httpVersion: String
    , headers: List HeaderEntry
    , statusCode: Int
    , statusMessage: String
    , body: Maybe String
    , start: Posix
    , end: Posix
    }


type CommunicateEvent
    = Request RequestData
    | Response ResponseData


decodeRequestResponse json =
    decodeString requestResponseDecoder json


requestResponseDecoder : Decoder CommunicateEvent
requestResponseDecoder =
    field "type" string
        |> andThen
            (\type_ ->
                case type_ of
                    "request" ->
                        requestDecoder
                    "response" ->
                        responseDecoder
                    _ ->
                        fail "hoge"
            )


requestDecoder : Decoder CommunicateEvent
requestDecoder =
    map8 RequestData
        (field "id" string)
        (field "httpVersion" string)
        (field "headers" (list headerEntryDecoder))
        (field "url" string)
        (field "method" string)
        (field "body" (nullable string))
        (field "start" posixDecoder)
        (field "end" posixDecoder)
        |> map Request


responseDecoder : Decoder CommunicateEvent
responseDecoder =
    map8 ResponseData
        (field "id" string)
        (field "httpVersion" string)
        (field "headers" (list headerEntryDecoder))
        (field "statusCode" int)
        (field "statusMessage" string)
        (field "body" (nullable string))
        (field "start" posixDecoder)
        (field "end" posixDecoder)
        |> map Response


headerEntryDecoder : Decoder HeaderEntry
headerEntryDecoder =
    map2 HeaderEntry
        (field "name" string)
        (field "value" string)


posixDecoder : Decoder Posix
posixDecoder =
    int |> map millisToPosix
