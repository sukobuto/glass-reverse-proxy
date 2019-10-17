module Main exposing (main)

import Browser
import Bulma.CDN exposing (stylesheet)
import Bulma.Modifiers as Mod exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Columns exposing (..)
import Bulma.Components exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Form exposing (connectedFields, controlInput, controlInputModifiers, field, label, control, controlModifiers)
import Bulma.Modifiers.Typography as Tg exposing (textWeight, Weight(..), textColor, textSize, textCentered)
import Cmd.Extra exposing (withCmd, withNoCmd, withCmds)
import Update.Extra exposing (andThen)
import Maybe.Extra as MaybeEx
import Html exposing (Attribute, Html, br, code, div, form, main_, p, span, strong, text)
import Html.Attributes exposing (action, class, id, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Decode exposing (Error(..))
import Task
import WebSocket exposing (Event(..))
import Monitor exposing (HeaderEntry, CommunicateEvent(..), RequestData, ResponseData, decodeRequestResponse)
import Time exposing (Posix, here, toHour, toMillis, toMinute, toSecond, utc)
import InfiniteList
import Browser.Events exposing (onResize)



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
    , requestAndResponseDisplayItems: List RequestAndResponse
    , requestAndResponseInfiniteList: InfiniteList.Model
    , viewingRequestAndResponse: Maybe RequestAndResponse
    , errorMsg : String
    , zone : Time.Zone
    , windowHeight : Int
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


type alias Flags =
    { windowHeight : Int }


init : Flags -> ( Model, Cmd Msg )
init flags =
    { socketInfo = Unopened
    , requestAndResponses = []
    , requestAndResponseDisplayItems = []
    , requestAndResponseInfiniteList = InfiniteList.init
    , viewingRequestAndResponse = Nothing
    , errorMsg = ""
    , zone = utc
    , windowHeight = flags.windowHeight
    }
    |> withCmds
        [ (WebSocket.connect "ws://localhost:8888" [ "echo-protocol" ])
        , Task.perform AdjustTimeZone Time.here
        ]



-- UPDATE


type Msg
    = SocketConnect WebSocket.ConnectionInfo
    | SocketReconnect
    | SocketClosed Int
    | ReceivedString String
    | SendString String
    | AdjustTimeZone Time.Zone
    | UpdateDisplayItems
    | InfiniteListMsg InfiniteList.Model
    | Error String
    | ViewRequestAndResponse RequestAndResponse
    | WindowResized Int Int
    | ClearRequestResponses
    | NoOp


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
            in
            { model | requestAndResponseDisplayItems = requestAndResponseDisplayItems }
            |> withCmd (
                InfiniteList.scrollToNthItem
                    { postScrollMessage = NoOp
                    , listHtmlId = "request-response-list"
                    , itemIndex = (List.length model.requestAndResponses) - 1
                    , configValue = (config model.windowHeight)
                    , items = (requestAndResponseDisplayItems |> List.map (Tuple.pair model))
                    }
            )

        InfiniteListMsg infiniteList ->
            { model | requestAndResponseInfiniteList = infiniteList }
            |> withNoCmd

        ViewRequestAndResponse requestAndResponse ->
            { model | viewingRequestAndResponse = Just requestAndResponse }
            |> withNoCmd

        ClearRequestResponses ->
            { model | requestAndResponses = [] }
            |> withNoCmd
            |> andThen update UpdateDisplayItems

        WindowResized w h ->
            { model | windowHeight = h }
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
subscriptions _ =
    Sub.batch
        [ WebSocket.events
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
        , onResize WindowResized
        ]




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
    widescreenContainer [ class "tool-bar" ]
        [ level []
            [ levelLeft [] (toolBarLeft model)
            , levelRight [] (toolBarRight model)
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
                [ button buttonModifiers
                    [ onClick ClearRequestResponses ]
                    [ text "CLEAR" ]
                ]
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
                    , button buttonModifiers [ onClick SocketReconnect ] [ text "CONNECT" ]
                    ]
        ]


requestResponseView : Model -> Html Msg
requestResponseView model =
    widescreenContainer []
        [ columns { columnsModifiers | gap = Gap0 } []
            [ column listColumnModifiers []
                [ requestResponseListView model ]
            , column detailColumnModifiers []
                [ requestResponseDetailView model ]
            ]
        ]


requestResponseDetailView : Model -> Html Msg
requestResponseDetailView model =
    div
        [ class "request-response-detail"
        , style "height" ((model.windowHeight - 50) |> px)
        ]
        (case model.viewingRequestAndResponse of
            Just requestAndResponse ->
                [ detailRequestView model requestAndResponse.requestData
                , (case requestAndResponse.responseData of
                    Just data -> detailResponseView model data
                    Nothing ->
                        box []
                            [ text "waiting response..." ]
                )
                ]
            Nothing ->
                [ hero { bold = False, size = Large, color = Mod.Light } []
                    [ heroBody []
                        [ container []
                            [ title H1 [ textColor Tg.GreyLighter, textCentered ] [ text "Glass Reverse Proxy" ] ]
                        ]
                    ]
                ]
        )


detailRequestView : Model -> RequestData -> Html Msg
detailRequestView model data =
    div [ class "detail-section" ]
        [ div [ class "section-name" ] [ text "REQUEST" ]
        , p [ style "margin-bottom" "4px" ]
            [ code []
                [ text data.method
                , text " "
                , text data.url
                , text " HTTP/"
                , text data.httpVersion
                ]
            ]
        , taggedInfo "start" (formatTime model.zone data.start)
        , headersView data.headers
        ]


headersView : List HeaderEntry -> Html Msg
headersView headers =
    table
        { bordered = True
        , striped = True
        , narrow = True
        , hoverable = False
        , fullWidth = True
        }
        [ class "headers-table" ]
        [ tableBody [] <| List.map
            (\h ->
                tableRow False []
                    [ tableCell [] [ text h.name ]
                    , tableCell [] [ text h.value ] ]
            )
            headers
        ]


--bodyView : String -> Html Msg
--bodyView body =
    -- TODO JSONデコードできたら JSON Viewer で表示
    -- できなければテキストボックスで表示


detailResponseView : Model -> ResponseData -> Html Msg
detailResponseView model data =
    div [ class "detail-section" ]
        [ div [ class "section-name" ] [ text "RESPONSE" ]
        , taggedInfo "status" ((String.fromInt data.statusCode) ++ data.statusMessage)
        ]


taggedInfo : String -> String -> Html Msg
taggedInfo name value =
    multitag []
        [ tag { tagModifiers | color = Mod.Dark } [] [ text name ]
        , tag { tagModifiers | color = Mod.Light } [] [ text value ]
        ]


listColumnModifiers : ColumnModifiers
listColumnModifiers =
    { offset = Auto
    , widths =
        { mobile = Just Auto
        , tablet = Just Width5
        , desktop = Just Width4
        , widescreen = Just Width3
        , fullHD = Just Width3
        }
    }


detailColumnModifiers : ColumnModifiers
detailColumnModifiers =
    { offset = Auto
    , widths =
        { mobile = Just Auto
        , tablet = Just Width7
        , desktop = Just Width8
        , widescreen = Just Width9
        , fullHD = Just Width9
        }
    }


requestResponseListView : Model -> Html Msg
requestResponseListView model =
    div [ class "infinite-list-container request-response-list"
        , InfiniteList.onScroll InfiniteListMsg
        , id "request-response-list"
        , style "height" ((model.windowHeight - 50) |> px)
        ]
        [ InfiniteList.view
            (config model.windowHeight)
            model.requestAndResponseInfiniteList
            (model.requestAndResponseDisplayItems |> List.map (Tuple.pair model)) ]


config : Int -> InfiniteList.Config ( Model, RequestAndResponse ) Msg
config windowHeight =
    InfiniteList.config
        { itemView = requestAndResponseListItemView
        , itemHeight = InfiniteList.withConstantHeight 60
        , containerHeight = windowHeight - 50
        }
        |> InfiniteList.withOffset 300
        |> InfiniteList.withClass "my-class"


requestAndResponseListItemView : Int -> Int -> ( Model, RequestAndResponse ) -> Html Msg
requestAndResponseListItemView _ _ item =
    let
        (model, requestAndResponse) = item
    in
    div
        [ class ("request-response-list__item" ++ (
                MaybeEx.filter (\x -> x == requestAndResponse) model.viewingRequestAndResponse
                    |> Maybe.map (\x -> " request-response-list__item--viewing")
                    |> Maybe.withDefault ""
            ))
        ]
        [ message
            { size = Standard
            , color =
                ( requestAndResponse.responseData
                  |> Maybe.map (\d -> d.statusCode)
                  |> statusToTagColor
                )
            }
            [ class "request-response-list__item__inner"
            , onClick (ViewRequestAndResponse requestAndResponse)
            ]
            [ messageBody []
                [ responseStatusBadge requestAndResponse.responseData
                , text " "
                , text (urlShorten requestAndResponse.requestData.url)
                , br [] []
                , span
                    [ textColor Tg.Grey
                    , textSize Tg.Small
                    ]
                    [ text (formatTime model.zone requestAndResponse.requestData.start) ]
                ]
            ]
        ]


responseStatusBadge : Maybe ResponseData -> Html Msg
responseStatusBadge responseData =
    case responseData of
        Just data ->
            span
                [ statusToTextColor (Just data.statusCode)
                , textWeight Bold
                ]
                [ text (data.statusCode |> String.fromInt) ]

        Nothing ->
            span
                [ statusToTextColor Nothing
                , textWeight Bold
                ]
                [ text "wait" ]


statusToTagColor : Maybe Int -> Mod.Color
statusToTagColor statusCode =
    case statusCode of
        Just code ->
            case (code // 100) of
                2 -> Mod.Success
                3 -> Mod.Info
                4 -> Mod.Warning
                5 -> Mod.Danger
                _ -> Mod.Light
        Nothing ->
            Mod.Light


statusToTextColor : Maybe Int -> Attribute msg
statusToTextColor statusCode =
    (case statusCode of
        Just code ->
            case (code // 100) of
                2 -> Tg.Success
                3 -> Tg.Info
                4 -> Tg.Warning
                5 -> Tg.Danger
                _ -> Tg.Grey
        Nothing ->
            Tg.GreyLight
    )
    |> textColor


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


px : Int -> String
px size =
    (String.fromInt size) ++ "px"


urlShorten : String -> String
urlShorten url =
    let
        lastIndexOfSlash =
            case (String.indices "/" url |> List.reverse |> List.head) of
                Just idx ->
                    idx + 1
                Nothing ->
                    0
        indexOfQuestionMark =
            case (String.indices "?" url |> List.reverse |> List.head) of
                Just idx ->
                    idx
                Nothing ->
                    -1
    in
        String.slice lastIndexOfSlash indexOfQuestionMark url
