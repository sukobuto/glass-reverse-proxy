module View.MonitorDetail exposing (..)


import JsonTree exposing (Node)
import Monitor exposing (getHeader, RequestOrResponse(..))
import Helpers exposing (formatTime, px, trimWithMarks, wrapWith)
import ModelAndMsg exposing (Model, Msg(..))

import Base64
import Bulma.Modifiers as Mod exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Form exposing (control, controlModifiers, controlTextArea, controlTextAreaModifiers, multilineFields)
import Bulma.Modifiers.Typography as Tg exposing (Weight(..), textColor, textCentered)
import Html exposing (Attribute, Html, code, div, img, p, text)
import Html.Attributes exposing (class, src, style, value)
import View.Spinner exposing (halfCircleSpinner)
import ViewModel exposing (DetailViewModel, ParseResult(..), TreeStates)



type BodyType
    = Plain
    | Html
    | Json
    | JavaScript
    | Css
    | Image String
    | Empty
    | Unknown String


bodyType : String -> BodyType
bodyType contentType =
    let
        type_ = trimWithMarks "" "/" contentType
        subtype = trimWithMarks "/" ";" contentType
    in
    case type_ of
        "text" ->
            case subtype of
                "plain" -> Plain
                "html" -> Html
                "css" -> Css
                _ -> Unknown contentType

        "application" ->
            case subtype of
                "javascript" -> JavaScript
                "json" -> Json
                _ -> Unknown contentType

        "image" ->
            Image subtype

        "" ->
            Empty

        _ ->
            Unknown contentType


-- VIEW


requestResponseDetailView : Model -> Html Msg
requestResponseDetailView model =
    div
        [ class "request-response-detail"
        , style "height" ((model.windowHeight - 58) |> px)
        ]
        (case model.detailViewModel of
            Just vm ->
                [ detailRequestView model vm
                , (case vm.requestAndResponse.responseData of
                    Just data -> detailResponseView model vm data
                    Nothing -> detailResponseWaitingView
                )
                ]
            Nothing ->
                [ container [ class "request-response-detail--nothing" ]
                    [ title H1 [ textColor Tg.GreyLighter, textCentered ] [ text "Glass Reverse Proxy" ] ]
                ]
        )


detailRequestView : Model -> DetailViewModel -> Html Msg
detailRequestView model vm =
    let
        data = vm.requestAndResponse.requestData
        contentType = getHeader "content-type" data.headers
            |> Maybe.withDefault ""
    in
    div [ class "detail-section" ] <| List.concat
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
          , tagListView
              [ ( "start", (formatTime model.zone data.start) ) ]
          , headersView data.headers
          ]
        , ( data.body
            |> Maybe.map
                (\body ->
                    case (bodyType contentType) of
                        Plain -> textBodyView body
                        Html -> textBodyView body
                        Json -> jsonBodyView (TreeViewStateUpdated Request) vm.requestBodyTreeStates body
                        JavaScript -> textBodyView body
                        Css -> textBodyView body
                        Image _ -> imageBodyView contentType body
                        Empty -> emptyBodyView
                        Unknown _ -> textBodyView body
                )
            |> Maybe.map (\x -> [x])
            |> Maybe.withDefault []
          )
        ]


detailResponseView : Model -> DetailViewModel -> Monitor.ResponseData -> Html Msg
detailResponseView model vm data =
    let
        contentType = getHeader "content-type" data.headers
            |> Maybe.withDefault ""
    in
    div [ class "detail-section" ] <| List.concat
        [
            [ div [ class "section-name" ] [ text "RESPONSE" ]
            , tagListView
                [ ("status", ((String.fromInt data.statusCode) ++ data.statusMessage))
                , ("end", (formatTime model.zone data.end))
                ]
            , headersView data.headers
            ]
            , ( data.body
              |> Maybe.map
                  (\body ->
                      case (bodyType contentType) of
                          Plain -> textBodyView body
                          Html -> textBodyView body
                          Json -> jsonBodyView (TreeViewStateUpdated Response) vm.responseBodyTreeStates body
                          JavaScript -> textBodyView body
                          Css -> textBodyView body
                          Image _ -> imageBodyView contentType body
                          Empty -> emptyBodyView
                          Unknown _ -> textBodyView body
                  )
              |> Maybe.map (\x -> [x])
              |> Maybe.withDefault []
            )
        ]


detailResponseWaitingView : Html Msg
detailResponseWaitingView =
    div []
        [ text "waiting response..."
        , halfCircleSpinner
        ]


headersView : List Monitor.HeaderEntry -> Html Msg
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


emptyBodyView : Html Msg
emptyBodyView =
    text ""


jsonBodyView : (JsonTree.State -> Msg) -> TreeStates -> String -> Html Msg
jsonBodyView msg treeStates body =
    let
        treeConfig =
            { onSelect = Nothing
            , toMsg = msg
            , colors = JsonTree.defaultColors
            }
    in
    case treeStates.parseResult of
        Parsed (Ok tree) ->
            JsonTree.view tree treeConfig treeStates.treeState
            |> wrapWith ( div [ style "font-size" "87%", style "overflow" "hidden" ] )

        Parsed (Err _) ->
            textBodyView body

        Parsing ->
            div []
                [ text "parsing..."
                , halfCircleSpinner
                ]



textBodyView : String -> Html Msg
textBodyView body =
    let
        modifiers =
            { controlTextAreaModifiers | readonly = True }
        controlAttrs = []
        textBody = Base64.decode body
            |> Result.withDefault ""
        textAreaAttrs =
            [ value textBody ]
    in
    controlTextArea modifiers controlAttrs textAreaAttrs []


imageBodyView : String -> String -> Html Msg
imageBodyView contentType image =
    let
        imageUrl = "data:" ++ contentType ++ ";base64," ++ image
    in
    img [ src imageUrl ] []


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
