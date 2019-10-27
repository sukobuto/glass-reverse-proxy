module ViewModel exposing (..)

import Base64
import Json.Decode as Decode
import JsonTree
import Monitor exposing (RequestOrResponse(..))
import Process
import Task exposing (Task, succeed)



type ParseResult
    = Parsed (Result Decode.Error JsonTree.Node)
    | Parsing


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



-- UPDATE HELPER


openDetail : Monitor.RequestAndResponse -> Maybe DetailViewModel
openDetail requestAndResponse =
    Just <|
        DetailViewModel requestAndResponse (initialTreeStates Parsing) (initialTreeStates Parsing)


parseJson : Monitor.RequestOrResponse -> Maybe DetailViewModel -> Task x ParseResult
parseJson reqOrRes detailViewModel =
    -- なぜか sleep を挟まないと非同期になっている感じがしない
    -- (クリックしてから DOM が更新されるまで 100ms ほど待たされる感じがある)
    Process.sleep 1
    |> Task.andThen
        (\_ ->
            case detailViewModel of
                Just vm ->
                    case reqOrRes of
                        Request ->
                            vm.requestAndResponse.requestData.body
                            |> Maybe.map (\body -> body |> Base64.decode |> Result.withDefault "cannot decode base64" )
                            |> Maybe.withDefault "no body"
                            |> JsonTree.parseString
                            |> Parsed
                            |> succeed

                        Response ->
                            vm.requestAndResponse.responseData
                            |> Maybe.andThen (\x -> x.body)
                            |> Maybe.map (\body -> body |> Base64.decode |> Result.withDefault "cannot decode base64" )
                            |> Maybe.withDefault "no body"
                            |> JsonTree.parseString
                            |> Parsed
                            |> succeed

                Nothing ->
                    succeed Parsing
        )


updateDetailTreeState : Monitor.RequestOrResponse -> JsonTree.State -> Maybe DetailViewModel -> Maybe DetailViewModel
updateDetailTreeState reqOrRes state detailViewModel =
    updateTreeStateHelper reqOrRes (\states -> { states | treeState = state }) detailViewModel


updateDetailTreeResult : Monitor.RequestOrResponse -> ParseResult -> Maybe DetailViewModel -> Maybe DetailViewModel
updateDetailTreeResult reqOrRes parseResult detailViewModel =
    updateTreeStateHelper reqOrRes (\states -> { states | parseResult = parseResult }) detailViewModel


updateTreeStateHelper : Monitor.RequestOrResponse -> (TreeStates -> TreeStates) -> Maybe DetailViewModel -> Maybe DetailViewModel
updateTreeStateHelper reqOrRes modifier detailViewModel =
    detailViewModel
    |> Maybe.map
        (\vm ->
            case reqOrRes of
                Monitor.Request ->
                    { vm | requestBodyTreeStates = ( modifier vm.requestBodyTreeStates ) }

                Monitor.Response ->
                    { vm | responseBodyTreeStates = ( modifier vm.responseBodyTreeStates ) }
        )
