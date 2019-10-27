module View.Layout exposing (..)

import Browser
import Bulma.Modifiers exposing (..)
import Bulma.Columns exposing (..)
import Bulma.Layout exposing (..)
import ModelAndMsg exposing (Model, Msg(..))
import View.MonitorDetail as MonitorDetail
import View.MonitorList as MonitorList
import Html exposing (Attribute, Html, main_)
import Html.Attributes exposing (class)
import View.ToolBar exposing (toolBar)


view : Model -> Browser.Document Msg
view model =
    let
        attrs =
            [ (if model.darkMode then [ class "inverted" ] else [])
            ]
            |> List.concat
    in
    { title = "GlassProxy"
    , body =
        [ main_ attrs
            [ toolBar model
            , requestResponseView model
            ]
        ]
    }


requestResponseView : Model -> Html Msg
requestResponseView model =
    fullHDContainer []
        [ columns { columnsModifiers | gap = Gap0 } []
            [ column listColumnModifiers []
                [ MonitorList.requestResponseListView model InfiniteListMsg ]
            , column detailColumnModifiers []
                [ MonitorDetail.requestResponseDetailView model ]
            ]
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
