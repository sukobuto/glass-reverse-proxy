module Main exposing (main)

import Browser
import Bulma.CDN exposing (stylesheet)
import Bulma.Modifiers exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Columns exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Form exposing (connectedFields, controlInput, controlInputModifiers, field, label, control, controlModifiers)
import Cmd.Extra exposing (withCmd, withNoCmd, withCmds)
import Html exposing (Html, main_, text, form, div, p)
import Html.Attributes exposing (class, value, type_, action)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Decode exposing (Error(..))
import Task
import WebSocket exposing (Event(..))
import Monitor exposing (HeaderEntry, CommunicateEvent(..), RequestData, ResponseData, decodeRequestResponse)
import Time exposing (Posix, here, toHour, toMillis, toMinute, toSecond, utc)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL

type alias Model =
    { socketInfo : SocketStatus
    , toSend : String
    , sentMessages : List String
    , receivedMessages : List String
    , requestAndResponses: List RequestAndResponse
    , errorMsg : String
    , zone : Time.Zone
    }


type SocketStatus
    = Unopened
    | Connected WebSocket.ConnectionInfo
    | Closed Int


type alias RequestAndResponse =
    { requestData: RequestData
    , responseData: Maybe ResponseData
    }


init : () -> ( Model, Cmd Msg )
init _ =
    { socketInfo = Unopened
    , toSend = ""
    , sentMessages = []
    , receivedMessages = []
    , requestAndResponses = []
    , errorMsg = ""
    , zone = utc
    }
    |> withCmds
        [ (WebSocket.connect "ws://localhost:8888" [ "echo-protocol" ])
        , Task.perform AdjustTimeZone Time.here
        ]



-- UPDATE


type Msg
    = SocketConnect WebSocket.ConnectionInfo
    | SocketClosed Int
    | SendStringChanged String
    | ReceivedString String
    | SendString
    | AdjustTimeZone Time.Zone
    | Error String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SocketConnect socketInfo ->
            { model | socketInfo = Connected socketInfo }
            |> withNoCmd

        SocketClosed unsentBytes ->
            { model | socketInfo = Closed unsentBytes }
            |> withNoCmd

        SendStringChanged string ->
            { model | toSend = string }
            |> withNoCmd

        SendString ->
            case model.socketInfo of
                Connected socketInfo ->
                    { model
                    | sentMessages = model.toSend :: model.sentMessages
                    , toSend = ""
                    }
                    |> withCmd ( WebSocket.sendString socketInfo model.toSend )

                _ ->
                    model
                    |> withNoCmd

        ReceivedString message ->
            case (decodeRequestResponse message) of
                Ok communicateEvent ->
                    case communicateEvent of
                        Request data ->
                            { model
                            | requestAndResponses =
                                { requestData = data, responseData = Nothing }
                                :: model.requestAndResponses
                            }
                            |> withNoCmd

                        Response data ->
                            { model
                            | requestAndResponses = (attachResponse model.requestAndResponses data)
                            }
                            |> withNoCmd

                _ ->
                    model |> withNoCmd

        AdjustTimeZone newZone ->
            { model | zone = newZone }
            |> withNoCmd

        Error errMsg ->
            { model | errorMsg = errMsg }
            |> withNoCmd


attachResponse : List RequestAndResponse -> ResponseData -> List RequestAndResponse
attachResponse requestAndResponses responseData =
    List.map (attachResponseHelper responseData) requestAndResponses

attachResponseHelper : ResponseData -> RequestAndResponse -> RequestAndResponse
attachResponseHelper responseData requestAndResponse =
    if requestAndResponse.requestData.id == responseData.id then
        { requestAndResponse | responseData = Just responseData }
    else
        requestAndResponse


-- SUBSCRIPTIONS


{-| Set up subscriptions and map socket events to app events. Because we are
only dealing with a single websocket connection, we can mostly ignore the connection
details and always assume data is coming in from the single open socket.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.events
        (\event ->
            case event of
                WebSocket.Connected info ->
                    SocketConnect info

                WebSocket.StringMessage _ message ->
                    ReceivedString message

                WebSocket.Closed _ unsentBytes ->
                    SocketClosed unsentBytes

                WebSocket.Error _ code ->
                    Error ("WebSocket Error: " ++ String.fromInt code)

                WebSocket.BadMessage error ->
                    Error error
        )



-- VIEW


view : Model -> Html Msg
view model =
    main_ []
        [ stylesheet
        , connectionState model
        , requestResponseView model.zone model.requestAndResponses
        ]


connectionState : Model -> Html Msg
connectionState model =
    container []
        [ case model.socketInfo of
            Unopened ->
                text "Connecting..."

            Connected info ->
                box []
                    [ text "Connected to"
                    , text info.url
                    ]

            Closed unsent ->
                box []
                    [ text " Closed with "
                    , text (String.fromInt unsent)
                    , text " bytes unsent."
                    ]
        ]


requestResponseView : Time.Zone -> List RequestAndResponse -> Html Msg
requestResponseView zone requestAndResponses =
    container []
        (List.map
            (\requestResponse ->
                requestResponseInfo zone requestResponse
            )
            requestAndResponses
        )


stringMsgControls : Model -> Html Msg
stringMsgControls model =
    container
        []
        [ form
            [ onSubmit SendString, action "javascript:void(0);" ]
            [ messageControls model ]
        , columns columnsModifiers []
            [ column columnModifiers []
                [ div [ class "sent" ]
                    (div [ class "header" ] [ text "Sent" ]
                        :: List.map messageInfo model.sentMessages
                    )
                ]
            , column columnModifiers []
                [ div [ class "received" ]
                    (div [ class "header" ] [ text "Received" ]
                        :: List.map messageInfo model.receivedMessages
                    )
                ]
            ]
        ]


requestResponseInfo : Time.Zone -> RequestAndResponse -> Html Msg
requestResponseInfo zone requestAndResponse =
    box []
    [ requestInfo zone requestAndResponse.requestData
    , case requestAndResponse.responseData of
        Just data ->
            responseInfo zone data
        Nothing ->
            noResponse
    ]


requestInfo : Time.Zone -> RequestData -> Html Msg
requestInfo zone data =
    div
    []
    [ p [] [text ("start: " ++ (formatTime zone data.start))]
    , p [] [text ("end: " ++ (formatTime zone data.end))]
    , p [] [text ("url: " ++ data.url)]
    ]


noResponse : Html Msg
noResponse =
    div
    []
    [ text "no response." ]


responseInfo : Time.Zone -> ResponseData -> Html Msg
responseInfo zone data =
    div
    []
    [ p [] [text ("start: " ++ (formatTime zone data.start))]
    , p [] [text ("end: " ++ (formatTime zone data.end))]
    , p [] [text ("status: " ++ (String.fromInt data.statusCode))]
    ]


formatTime : Time.Zone -> Posix -> String
formatTime zone time =
    let
        hour = String.fromInt (toHour zone time)
        minute = String.fromInt (toMinute zone time)
        second = String.fromInt (toSecond zone time)
        millis = String.fromInt (toMillis zone time)
    in
    hour ++ ":" ++ minute ++ ":" ++ second ++ "." ++ millis


messageControls : Model -> Html Msg
messageControls model =
    connectedFields Left []
        [ controlInput controlInputModifiers [] [ onInput SendStringChanged, value model.toSend ] []
        , control controlModifiers []
            [ button buttonModifiers [ type_ "submit" ] [ text "Send" ] ]
        ]


messageInfo : String -> Html Msg
messageInfo message =
    div [] [ text message ]
