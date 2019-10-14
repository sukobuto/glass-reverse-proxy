module Main exposing (main)

import Browser
import Bulma.CDN exposing (stylesheet)
import Bulma.Modifiers exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Columns exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Form exposing (connectedFields, controlInput, controlInputModifiers, field, label, control, controlModifiers)
import Cmd.Extra exposing (withCmd, withNoCmd)
import Html exposing (Html, main_, text, form, div)
import Html.Attributes exposing (class, value, type_, action)
import Html.Events exposing (onClick, onInput, onSubmit)
import WebSocket exposing (Event(..))


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
    , errorMsg : String
    }


type SocketStatus
    = Unopened
    | Connected WebSocket.ConnectionInfo
    | Closed Int


init : () -> ( Model, Cmd Msg )
init _ =
    { socketInfo = Unopened
    , toSend = ""
    , sentMessages = []
    , receivedMessages = []
    , errorMsg = ""
    }
    |> withCmd (WebSocket.connect "ws://localhost:8888" [ "echo-protocol" ])



-- UPDATE


type Msg
    = SocketConnect WebSocket.ConnectionInfo
    | SocketClosed Int
    | SendStringChanged String
    | ReceivedString String
    | SendString
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
            { model | receivedMessages = message :: model.receivedMessages }
            |> withNoCmd

        Error errMsg ->
            { model | errorMsg = errMsg }
            |> withNoCmd


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
        , stringMsgControls model
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
