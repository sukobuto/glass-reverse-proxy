module View.Spinner exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (class)


halfCircleSpinner : Html msg
halfCircleSpinner =
    div [ class "half-circle-spinner" ]
        [ div [ class "circle circle-1" ] []
        , div [ class "circle circle-2" ] []
        ]
