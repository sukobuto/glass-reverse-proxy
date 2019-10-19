module View.Atom exposing (..)


import Bulma.Elements exposing (icon)
import Bulma.Modifiers exposing (Size(..))
import Html exposing (Attribute, Html, i)
import Html.Attributes exposing (class)
import ModelAndMsg exposing (Msg)



faIconButton : List (Attribute Msg) -> String -> Html Msg
faIconButton attributes iconName =
    let
        attrs =
            attributes
            |> List.append
                [ class "icon-button" ]
    in
    icon Large attrs
        [ i [ class ("fas fa-" ++ iconName) ] [] ]
