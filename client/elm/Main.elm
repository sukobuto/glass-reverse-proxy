module Main exposing (main)

import Browser
import Bulma.CDN exposing (stylesheet)
import Bulma.Modifiers exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Columns exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Form exposing (connectedFields, controlInput, controlInputModifiers, field, label, control, controlModifiers)
import Cmd.Extra exposing (withCmd, withNoCmd, withCmds)
import Update.Extra exposing (andThen)
import Html exposing (Html, main_, text, form, div, p, strong)
import Html.Attributes exposing (action, class, id, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Decode exposing (Error(..))
import Task
import WebSocket exposing (Event(..))
import Monitor exposing (HeaderEntry, CommunicateEvent(..), RequestData, ResponseData, decodeRequestResponse)
import Time exposing (Posix, here, toHour, toMillis, toMinute, toSecond, utc)
import InfiniteList



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
    , requestAndResponses: List RequestAndResponse
    , requestAndResponseDisplayItems: List RequestAndResponseWithZone
    , requestAndResponseInfiniteList: InfiniteList.Model
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


type alias RequestAndResponseWithZone =
    { zone: Time.Zone
    , requestData: RequestData
    , responseData: Maybe ResponseData
    }


init : () -> ( Model, Cmd Msg )
init _ =
    { socketInfo = Unopened
    , requestAndResponses = []
    , requestAndResponseDisplayItems = []
    , requestAndResponseInfiniteList = InfiniteList.init
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
    | ReceivedString String
    | SendString String
    | AdjustTimeZone Time.Zone
    | UpdateDisplayItems
    | InfiniteListMsg InfiniteList.Model
    | Error String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SocketConnect socketInfo ->
            { model | socketInfo = Connected socketInfo }
            |> withNoCmd

        SocketClosed unsentBytes ->
            { model | socketInfo = Closed unsentBytes }
            |> withNoCmd

        SendString message ->
            case model.socketInfo of
                Connected socketInfo ->
                    model
                    |> withCmd ( WebSocket.sendString socketInfo message )

                _ ->
                    model
                    |> withNoCmd

        ReceivedString message ->
            case (decodeRequestResponse message) of
                Ok communicateEvent ->
                    case communicateEvent of
                        Request data ->
                            let
                                requestAndResponses =
                                    { requestData = data, responseData = Nothing }
                                    :: model.requestAndResponses
                            in
                            { model | requestAndResponses = requestAndResponses }
                            |> withNoCmd
                            |> andThen update UpdateDisplayItems

                        Response data ->
                            { model | requestAndResponses = (attachRequestToResponse model.requestAndResponses data) }
                            |> withNoCmd
                            |> andThen update UpdateDisplayItems

                _ ->
                    model |> withNoCmd

        AdjustTimeZone newZone ->
            { model | zone = newZone }
            |> withNoCmd

        Error errMsg ->
            { model | errorMsg = errMsg }
            |> withNoCmd

        UpdateDisplayItems ->
            let
                requestAndResponseDisplayItems =
                    model.requestAndResponses
                    |> List.reverse
                    |> List.map (attachZoneHelper model.zone)
            in
            { model | requestAndResponseDisplayItems = requestAndResponseDisplayItems }
            |> withCmd (
                InfiniteList.scrollToNthItem
                    { postScrollMessage = NoOp
                    , listHtmlId = "request-response-list"
                    , itemIndex = (List.length model.requestAndResponses) - 1
                    , configValue = config
                    , items = requestAndResponseDisplayItems
                    }
            )

        InfiniteListMsg infiniteList ->
            { model | requestAndResponseInfiniteList = infiniteList }
            |> withNoCmd

        NoOp ->
            model
            |> withNoCmd


attachRequestToResponse : List RequestAndResponse -> ResponseData -> List RequestAndResponse
attachRequestToResponse requestAndResponses responseData =
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
        , toolBar model
        , requestResponseView model
        ]


toolBar : Model -> Html Msg
toolBar model =
    section NotSpaced []
        [ fluidContainer []
            [ level []
                [ levelLeft [] (toolBarLeft model)
                , levelRight [] (toolBarRight model)
                ]
            ]
        ]


toolBarLeft : Model -> List (Html Msg)
toolBarLeft model =
    [ levelItemText []
        [ strong [] [ text (String.fromInt <| List.length model.requestAndResponses) ]
        , text "requests"
        ]
    , levelItem []
        [ field []
            [ control controlModifiers []
                [ button buttonModifiers [ ] [ text "CLEAR" ] ]
            ]
        ]
    ]


toolBarRight : Model -> List (Html Msg)
toolBarRight model =
    [ levelItemText []
        [ connectionState model ]
    ]


connectionState : Model -> Html Msg
connectionState model =
    div []
        [ case model.socketInfo of
            Unopened ->
                text "Connecting..."

            Connected info ->
                div []
                    [ text "Connected to "
                    , text info.url
                    ]

            Closed unsent ->
                div []
                    [ text " Closed with "
                    , text (String.fromInt unsent)
                    , text " bytes unsent."
                    ]
        ]


requestResponseView : Model -> Html Msg
requestResponseView model =
    section NotSpaced []
        [ fluidContainer []
            [ requestResponseListView model ]
        ]


requestResponseListView : Model -> Html Msg
requestResponseListView model =
    div
        [ class "infinite-list-container request-response-list"
        , InfiniteList.onScroll InfiniteListMsg
        , id "request-response-list"
        ]
        [ InfiniteList.view config model.requestAndResponseInfiniteList model.requestAndResponseDisplayItems ]


config : InfiniteList.Config RequestAndResponseWithZone Msg
config =
    InfiniteList.config
        { itemView = requestAndResponseItemView
        , itemHeight = InfiniteList.withConstantHeight 160
        , containerHeight = 500
        }
        |> InfiniteList.withOffset 300
        |> InfiniteList.withClass "my-class"


requestAndResponseItemView : Int -> Int -> RequestAndResponseWithZone -> Html Msg
requestAndResponseItemView idx listIdx item =
    div [ class "request-response-list__item" ]
        [ requestInfo item.zone item.requestData
        , case item.responseData of
            Just data ->
                responseInfo item.zone data
            Nothing ->
                noResponse
        ]


requestResponseInfo : Time.Zone -> RequestAndResponse -> Html Msg
requestResponseInfo zone requestAndResponse =
    div [ class "request-response-list-item" ]
        [ requestInfo zone requestAndResponse.requestData
        , case requestAndResponse.responseData of
            Just data ->
                responseInfo zone data
            Nothing ->
                noResponse
        ]


requestInfo : Time.Zone -> RequestData -> Html Msg
requestInfo zone data =
    div []
        [ p [] [text ("start: " ++ (formatTime zone data.start))]
        , p [] [text ("end: " ++ (formatTime zone data.end))]
        , p [] [text ("url: " ++ data.url)]
        ]


noResponse : Html Msg
noResponse =
    div []
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


-- HELPERS


attachZoneHelper : Time.Zone -> RequestAndResponse -> RequestAndResponseWithZone
attachZoneHelper zone requestAndResponse =
    RequestAndResponseWithZone zone requestAndResponse.requestData requestAndResponse.responseData
