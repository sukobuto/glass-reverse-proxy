module Main exposing (main)

import Browser
import Bulma.CDN exposing (stylesheet)
import Bulma.Modifiers as Mod exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Columns exposing (..)
import Bulma.Components exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Form exposing (connectedFields, control, controlInput, controlInputModifiers, controlModifiers, controlTextArea, controlTextAreaModifiers, field, horizontalFields, label, multilineFields)
import Bulma.Modifiers.Typography as Tg exposing (textWeight, Weight(..), textColor, textSize, textCentered)
import Cmd.Extra exposing (withCmd, withNoCmd, withCmds)
import Update.Extra exposing (andThen)
import Maybe.Extra as MaybeEx
import Html exposing (Attribute, Html, br, code, div, form, main_, p, span, strong, text)
import Html.Attributes exposing (action, class, id, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Decode as Decode exposing (Error(..))
import Task
import WebSocket exposing (Event(..))
import Monitor exposing (HeaderEntry, CommunicateEvent(..), RequestData, ResponseData, decodeRequestResponse)
import Time exposing (Posix, here, toHour, toMillis, toMinute, toSecond, utc)
import InfiniteList
import Browser.Events exposing (onResize)
import JsonTree exposing (Node)



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
    , detailViewModel: Maybe DetailViewModel
    , errorMsg : String
    , zone : Time.Zone
    , windowHeight : Int
    }


type SocketStatus
    = Unopened
    | Connected WebSocket.ConnectionInfo
    | Closed Int


type alias ParseResult = Result Decode.Error JsonTree.Node


type alias TreeStates =
    { parseResult : ParseResult
    , treeState : JsonTree.State
    , selections : List JsonTree.KeyPath
    }


type alias RequestAndResponseDetailStates =
    { requestBody : TreeStates
    , responseBody : TreeStates
    }


type alias RequestAndResponse =
    { requestData: RequestData
    , responseData: Maybe ResponseData
    }


type alias DetailViewModel =
    { requestAndResponse : RequestAndResponse
    , requestBodyTreeStates : TreeStates
    , responseBodyTreeStates : TreeStates
    }


initialTreeStates : ParseResult -> TreeStates
initialTreeStates parseResult =
    { parseResult = parseResult
    , treeState = JsonTree.defaultState
    , selections = []
    }


detailViewModel : (ParseResult -> TreeStates) -> (ParseResult -> TreeStates) -> RequestAndResponse -> DetailViewModel
detailViewModel requestBodyTreeStates responseBodyTreeStates requestAndResponse =
    let
        requestBodyParseResult =
            requestAndResponse.requestData.body
            |> Maybe.withDefault ""
            |> JsonTree.parseString
        responseBodyParseResult =
            requestAndResponse.responseData
            |> Maybe.andThen (\x -> x.body)
            |> Maybe.withDefault ""
            |> JsonTree.parseString
    in
    { requestAndResponse = requestAndResponse
    , requestBodyTreeStates = requestBodyTreeStates requestBodyParseResult
    , responseBodyTreeStates = responseBodyTreeStates responseBodyParseResult
    }


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
    | SetRequestBodyTreeViewState JsonTree.State
    | SetResponseBodyTreeViewState JsonTree.State
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
                                    { requestData = data
                                    , responseData = Nothing
                                    }
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
            { model | detailViewModel = Just (detailViewModel initialTreeStates initialTreeStates requestAndResponse) }
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
            case model.detailViewModel of
                Just vm ->
                    { model | detailViewModel =
                        Just
                            ( detailViewModel
                                (\r ->
                                    { parseResult = r
                                    , treeState = state
                                    , selections = []
                                    }
                                )
                                (\_ -> vm.responseBodyTreeStates)
                                vm.requestAndResponse
                            )
                    }
                    |> withNoCmd
                Nothing ->
                    model
                    |> withNoCmd

        SetResponseBodyTreeViewState state ->
            case model.detailViewModel of
                Just vm ->
                    { model | detailViewModel =
                        Just
                            ( detailViewModel
                                (\_ -> vm.requestBodyTreeStates)
                                (\r ->
                                    { parseResult = r
                                    , treeState = state
                                    , selections = []
                                    }
                                )
                                vm.requestAndResponse
                            )
                    }
                    |> withNoCmd
                Nothing ->
                    model
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
        (case model.detailViewModel of
            Just vm ->
                [ detailRequestView model vm
                , (case vm.requestAndResponse.responseData of
                    Just data -> detailResponseView model vm data
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


detailRequestView : Model -> DetailViewModel -> Html Msg
detailRequestView model vm =
    let
        data = vm.requestAndResponse.requestData
    in
    div [ class "detail-section" ]
        <| List.concat
            [ [ div [ class "section-name" ] [ text "REQUEST" ]
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
            , (case data.body of
                  Just body -> [bodyView SetRequestBodyTreeViewState vm.requestBodyTreeStates body]
                  Nothing -> []
              )
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


bodyView : (JsonTree.State -> Msg) -> TreeStates -> String -> Html Msg
bodyView msg treeStates body =
    let
        treeConfig =
            { onSelect = Nothing
            , toMsg = msg
            , colors = JsonTree.defaultColors
            }
    in
    treeStates.parseResult
        |> Result.map (\tree -> JsonTree.view tree treeConfig treeStates.treeState)
        |> Result.withDefault (textBodyView body)


textBodyView : String -> Html Msg
textBodyView body =
    let
        modifiers =
            { controlTextAreaModifiers | readonly = True }
        controlAttrs = []
        textAreaAttrs =
            [ value body ]
    in
    controlTextArea modifiers controlAttrs textAreaAttrs []



detailResponseView : Model -> DetailViewModel -> ResponseData -> Html Msg
detailResponseView model vm data =
    div [ class "detail-section" ] <| List.concat
        [
            [ div [ class "section-name" ] [ text "RESPONSE" ]
            , tagListView
                [ ("status", ((String.fromInt data.statusCode) ++ data.statusMessage))
                , ("end", (formatTime model.zone data.end))
                ]
            , headersView data.headers
            ]
            , (case data.body of
                Just body -> [bodyView SetResponseBodyTreeViewState vm.responseBodyTreeStates body]
                Nothing -> []
            )
        ]


tagListView : List (String, String) -> Html Msg
tagListView items =
    multilineFields []
        ( List.map
            (\item ->
                let
                    (name, value) = item
                in
                control controlModifiers []
                    [ taggedInfo name value ]
            )
            items
        )


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
                MaybeEx.filter (\x -> x.requestAndResponse == requestAndResponse) model.detailViewModel
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
