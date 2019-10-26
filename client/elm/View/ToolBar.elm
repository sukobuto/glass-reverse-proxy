module View.ToolBar exposing (..)

import Bulma.Elements exposing (button, buttonModifiers)
import Bulma.Form exposing (control, controlModifiers, field)
import Bulma.Layout exposing (fullHDContainer, level, levelItem, levelItemText, levelLeft, levelRight)
import Helpers exposing (wrapWith)
import Html exposing (Html, div, strong, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import ModelAndMsg exposing (Model, Msg(..), SocketStatus(..))
import View.Atom exposing (faIconButton)



toolBar : Model -> Html Msg
toolBar model =
    fullHDContainer []
        [ level []
            [ levelLeft [] (toolBarLeft model)
            , levelRight [] (toolBarRight model)
            ]
        ]
    |> wrapWith (div [ class "tool-bar" ])


toolBarLeft : Model -> List (Html Msg)
toolBarLeft model =
    [ levelItemText []
        [ strong [] [ text (String.fromInt <| List.length model.requestAndResponses) ]
        , text "requests"
        ]
    ]
    |> List.append
        (case ( List.head model.requestAndResponses ) of
            Just _ ->
                [ levelItem []
                    [ field []
                        [ control controlModifiers []
                            [ button buttonModifiers
                                [ onClick ClearRequestResponses ]
                                [ text "CLEAR" ]
                            ]
                        ]
                    ]
                ]

            Nothing -> []
        )


toolBarRight : Model -> List (Html Msg)
toolBarRight model =
    [ levelItem []
        [ field []
            [ control controlModifiers []
                [ faIconButton [ onClick ToggleDarkMode ] "adjust" ]
            ]
        ]
    , levelItemText []
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
