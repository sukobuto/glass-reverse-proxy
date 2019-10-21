module ViewModel exposing (..)

import Base64
import Json.Decode as Decode
import JsonTree
import Monitor



type alias ParseResult = Result Decode.Error JsonTree.Node


type alias TreeStates =
    { parseResult : ParseResult
    , treeState : JsonTree.State
    , selections : List JsonTree.KeyPath
    }


type alias DetailViewModel =
    { requestAndResponse : Monitor.RequestAndResponse
    , requestBodyTreeStates : TreeStates
    , responseBodyTreeStates : TreeStates
    }



initialTreeStates : ParseResult -> TreeStates
initialTreeStates parseResult =
    { parseResult = parseResult
    , treeState = JsonTree.defaultState
    , selections = []
    }


makeDetailViewModel : (ParseResult -> TreeStates) -> (ParseResult -> TreeStates) -> Monitor.RequestAndResponse -> DetailViewModel
makeDetailViewModel requestBodyTreeStates responseBodyTreeStates requestAndResponse =
    let
        requestBodyParseResult =
            requestAndResponse.requestData.body
            |> Maybe.withDefault ""
            |> Base64.decode
            |> Result.withDefault ""
            |> JsonTree.parseString
        responseBodyParseResult =
            requestAndResponse.responseData
            |> Maybe.andThen (\x -> x.body)
            |> Maybe.withDefault ""
            |> Base64.decode
            |> Result.withDefault ""
            |> JsonTree.parseString
    in
    { requestAndResponse = requestAndResponse
    , requestBodyTreeStates = requestBodyTreeStates requestBodyParseResult
    , responseBodyTreeStates = responseBodyTreeStates responseBodyParseResult
    }



-- UPDATE HELPER


openDetail : Monitor.RequestAndResponse -> Maybe DetailViewModel
openDetail requestAndResponse =
    Just ( makeDetailViewModel initialTreeStates initialTreeStates requestAndResponse )


updateRequestBodyTreeViewState : Maybe DetailViewModel -> JsonTree.State -> Maybe DetailViewModel
updateRequestBodyTreeViewState detailViewModel state =
    detailViewModel
        |> Maybe.map
            (\vm ->
                makeDetailViewModel
                    (\r ->
                        { parseResult = r
                        , treeState = state
                        , selections = vm.requestBodyTreeStates.selections
                        }
                    )
                    (always vm.responseBodyTreeStates)
                    vm.requestAndResponse
            )


updateResponseBodyTreeViewState : Maybe DetailViewModel -> JsonTree.State -> Maybe DetailViewModel
updateResponseBodyTreeViewState detailViewModel state =
    detailViewModel
        |> Maybe.map
            (\vm ->
                makeDetailViewModel
                    (always vm.requestBodyTreeStates)
                    (\r ->
                        { parseResult = r
                        , treeState = state
                        , selections = vm.responseBodyTreeStates.selections
                        }
                    )
                    vm.requestAndResponse
            )