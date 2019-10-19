module Main exposing (main)

import Browser
import Cmd.Extra exposing (withCmd, withNoCmd, withCmds)
import Update.Extra exposing (andThen)
import Json.Decode as Decode exposing (Error(..))
import Task
import ViewModel exposing (openDetail, updateRequestBodyTreeViewState, updateResponseBodyTreeViewState)
import WebSocket
import Time exposing (Posix, utc)
import InfiniteList
import Browser.Events exposing (onResize)

import ModelAndMsg exposing (Model, SocketStatus(..), Msg(..))
import Monitor exposing (HeaderEntry, CommunicateEvent(..), RequestData, ResponseData, decodeRequestResponse, RequestAndResponse)
import View.Layout
import View.MonitorList



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = View.Layout.view
        }



-- MODEL


type alias Flags =
    { windowHeight : Int }


init : Flags -> ( Model, Cmd Msg )
init flags =
    { socketInfo = Unopened
    , requestAndResponses = []
    , requestAndResponseDisplayItems = []
    , requestAndResponseInfiniteList = InfiniteList.init
    , detailViewModel = Nothing
    , errorMsg = ""
    , zone = utc
    , windowHeight = flags.windowHeight
    , darkMode = False
    }
    |> withCmds
        [ (WebSocket.connect "ws://localhost:8888" [ "echo-protocol" ])
        , Task.perform AdjustTimeZone Time.here
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SocketConnect socketInfo ->
            { model | socketInfo = Connected socketInfo }
            |> withNoCmd

        SocketReconnect ->
            model |> withCmd (WebSocket.connect "ws://localhost:8888" [ "echo-protocol" ])

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
                    let
                        requestAndResponses =
                            Monitor.updateRequestAndResponseList model.requestAndResponses communicateEvent
                    in
                    { model | requestAndResponses = requestAndResponses }
                    |> withNoCmd
                    |> andThen update UpdateDisplayItems

                Err e ->
                    { model | errorMsg = (Decode.errorToString e) }
                    |> withNoCmd

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
                newModel =
                    { model | requestAndResponseDisplayItems = requestAndResponseDisplayItems }
            in
            if (List.length newModel.requestAndResponseDisplayItems) > (List.length model.requestAndResponseDisplayItems) then
                newModel
                |> withCmd (View.MonitorList.scrollToBottom newModel NoOp)
            else
                newModel
                |> withNoCmd

        InfiniteListMsg infiniteList ->
            { model | requestAndResponseInfiniteList = infiniteList }
            |> withNoCmd

        ViewRequestAndResponse requestAndResponse ->
            { model | detailViewModel = (openDetail requestAndResponse) }
            |> withNoCmd

        ClearRequestResponses ->
            { model |
              requestAndResponses = []
            , detailViewModel = Nothing
            }
            |> withNoCmd
            |> andThen update UpdateDisplayItems

        WindowResized _ h ->
            { model | windowHeight = h }
            |> withNoCmd

        SetRequestBodyTreeViewState state ->
            { model |
              detailViewModel =
                  ( updateRequestBodyTreeViewState model.detailViewModel state )
            }
            |> withNoCmd

        SetResponseBodyTreeViewState state ->
            { model |
              detailViewModel =
                  ( updateResponseBodyTreeViewState model.detailViewModel state )
            }
            |> withNoCmd

        ToggleDarkMode ->
            { model | darkMode = not model.darkMode }
            |> withNoCmd

        NoOp ->
            model
            |> withNoCmd



-- SUBSCRIPTIONS


{-| Set up subscriptions and map socket events to app events. Because we are
only dealing with a single websocket connection, we can mostly ignore the connection
details and always assume data is coming in from the single open socket.
-}
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ WebSocket.events
            (\event ->
                case event of
                    WebSocket.Connected info -> SocketConnect info
                    WebSocket.StringMessage _ message -> ReceivedString message
                    WebSocket.Closed _ unsentBytes -> SocketClosed unsentBytes
                    WebSocket.Error _ code -> Error ("WebSocket Error: " ++ String.fromInt code)
                    WebSocket.BadMessage error -> Error error
            )
        , onResize WindowResized
        ]
