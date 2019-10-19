module View.MonitorDetail exposing (..)


import JsonTree exposing (Node)
import Monitor
import Helpers exposing (formatTime, px, wrapElement)
import ModelAndMsg exposing (Model, Msg(..))


import Bulma.Modifiers as Mod exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Form exposing (control, controlModifiers, controlTextArea, controlTextAreaModifiers, multilineFields)
import Bulma.Modifiers.Typography as Tg exposing (Weight(..), textColor, textCentered)
import Html exposing (Attribute, Html, code, div, p, text)
import Html.Attributes exposing (class, style, value)
import ViewModel exposing (DetailViewModel, ParseResult, TreeStates)



-- VIEW


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
                [ container [ class "request-response-detail--nothing" ]
                    [ title H1 [ textColor Tg.GreyLighter, textCentered ] [ text "Glass Reverse Proxy" ] ]
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
        |> wrapElement ( div [ style "font-size" "87%", style "overflow" "hidden" ] )


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



detailResponseView : Model -> DetailViewModel -> Monitor.ResponseData -> Html Msg
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
