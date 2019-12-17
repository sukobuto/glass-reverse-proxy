module ModelAndMsg exposing (..)

import Browser
import Time
import Monitor
import InfiniteList
import Url
import WebSocket
import JsonTree
import ViewModel


type alias Model =
    { location : Url.Url
    , wsUrl : String
    , socketInfo : SocketStatus
    , requestAndResponses: List Monitor.RequestAndResponse
    , requestAndResponseDisplayItems: List Monitor.RequestAndResponse
    , requestAndResponseInfiniteList: InfiniteList.Model
    , detailViewModel: Maybe ViewModel.DetailViewModel
    , errorMsg : String
    , zone : Time.Zone
    , windowHeight : Int
    , darkMode : Bool
    }


type SocketStatus
    = Unopened
    | Connected WebSocket.ConnectionInfo
    | Closed Int


type Msg
    = NoOp
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | SocketConnect WebSocket.ConnectionInfo
    | SocketReconnect
    | SocketClosed Int
    | ReceivedString String
    | SendString String
    | AdjustTimeZone Time.Zone
    | UpdateDisplayItems
    | InfiniteListMsg InfiniteList.Model
    | Error String
    | ShowDetail Monitor.RequestAndResponse
    | UpdateDetail
    | TreeViewParsed Monitor.RequestOrResponse ViewModel.ParseResult
    | TreeViewStateUpdated Monitor.RequestOrResponse JsonTree.State
    | WindowResized Int Int
    | ClearRequestResponses
    | ToggleDarkMode
