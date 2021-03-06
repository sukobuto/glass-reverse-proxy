module View.MonitorList exposing (..)

import Bulma.Components exposing (message, messageBody)
import Bulma.Modifiers as Mod exposing (Size(..))
import Bulma.Modifiers.Typography as Tg exposing (Weight(..), textColor, textSize, textWeight)
import Maybe.Extra as MaybeEx
import ModelAndMsg exposing (Model, Msg(..))
import Monitor exposing (RequestData, ResponseData)
import InfiniteList
import Html exposing (Attribute, Html, br, code, div, span, text)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
import Helpers exposing (formatTime, posixDiff, px, trimWithMarks, wrapWith)
import Monitor exposing (RequestAndResponse, getHeader)
import Time exposing (Posix)


-- MODEL


requestResponseListView : Model -> (InfiniteList.Model -> Msg) -> Html Msg
requestResponseListView model infiniteListMsg=
    let
        attrs =
            [ class "infinite-list-container request-response-list"
            , InfiniteList.onScroll infiniteListMsg
            , id "request-response-list"
            , style "height" ((model.windowHeight - 58) |> px)
            ]
    in
    div attrs
        [ InfiniteList.view
            (config model.windowHeight)
            model.requestAndResponseInfiniteList
            (model.requestAndResponseDisplayItems |> List.map (Tuple.pair model)) ]


config : Int -> InfiniteList.Config ( Model, RequestAndResponse ) Msg
config windowHeight =
    InfiniteList.config
        { itemView = requestAndResponseListItemView
        , itemHeight = InfiniteList.withConstantHeight 60
        , containerHeight = windowHeight - 58
        }
        |> InfiniteList.withOffset 300
        |> InfiniteList.withClass "my-class"


requestAndResponseListItemView : Int -> Int -> ( Model, RequestAndResponse ) -> Html Msg
requestAndResponseListItemView _ _ item =
    let
        (model, requestAndResponse) = item
        attrs =
            [ class "request-response-list__item" ]
            |> List.append
                ( MaybeEx.filter (\x -> x.requestAndResponse == requestAndResponse) model.detailViewModel
                    |> Maybe.map (always [ class "request-response-list__item--viewing" ])
                    |> Maybe.withDefault []
                )
    in
    div attrs
        [ message
            { size = Standard
            , color =
                ( requestAndResponse.responseData
                  |> Maybe.map (\d -> d.statusCode)
                  |> statusToTagColor
                )
            }
            [ class "request-response-list__item__inner"
            , onClick (ShowDetail requestAndResponse)
            ]
            [ messageBody []
                [ span [] [ text ( requestAndResponse.requestData.method ++ " " ) ]
                , text ( trimWithMarks "/" "?" requestAndResponse.requestData.url )
                , br [] []
                , ( case requestAndResponse.responseData of
                      Just data -> responseInfo requestAndResponse.requestData.start data
                      Nothing -> responseWaiting
                  )
                ]
            ]
        ]


responseWaiting : Html Msg
responseWaiting =
    span
        [ statusToTextColor Nothing
        , textWeight Bold
        ]
        [ text "WAIT" ]


responseInfo : Posix -> ResponseData -> Html Msg
responseInfo startTime data =
    let
        elapsed = ( posixDiff startTime data.end |> String.fromInt ) ++ "ms"
        elapsedPad = String.repeat (7 - ( String.length elapsed )) "0"
    in
    div []
        [ span
            [ statusToTextColor ( Just data.statusCode )
            , textWeight Bold
            ]
            [ text ( data.statusCode |> String.fromInt ) ]
        , text " "
        , code
            [ class "response-info-detail-item"
            , style "white-space" "pre-wrap"
            ]
            [ span [ class "elapsed-pad" ] [ text elapsedPad ]
            , text elapsed
            ]
        , text " "
        , code
            [ class "response-info-detail-item" ]
            [ text
                ( data.headers
                    |> getHeader "content-type"
                    |> Maybe.map ( trimWithMarks "" ";" )
                    |> Maybe.withDefault "..."
                )
            ]
        ]


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


statusToTextColor : Maybe Int -> Attribute Msg
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



-- HELPERS


scrollToBottom : Model -> Msg -> Cmd Msg
scrollToBottom model msg =
    InfiniteList.scrollToNthItem
        { postScrollMessage = msg
        , listHtmlId = "request-response-list"
        , itemIndex = (List.length model.requestAndResponses) - 1
        , configValue = (config model.windowHeight)
        , items = (model.requestAndResponseDisplayItems |> List.map (Tuple.pair model))
        }
