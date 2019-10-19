module ModelAndMsg exposing (..)

import Time
import Monitor
import InfiniteList
import WebSocket
import JsonTree
import ViewModel


type alias Model =
    { socketInfo : SocketStatus
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
    | SocketConnect WebSocket.ConnectionInfo
    | SocketReconnect
    | SocketClosed Int
    | ReceivedString String
    | SendString String
    | AdjustTimeZone Time.Zone
    | UpdateDisplayItems
    | InfiniteListMsg InfiniteList.Model
    | Error String
    | ViewRequestAndResponse Monitor.RequestAndResponse
    | SetRequestBodyTreeViewState JsonTree.State
    | SetResponseBodyTreeViewState JsonTree.State
    | WindowResized Int Int
    | ClearRequestResponses
    | ToggleDarkMode
