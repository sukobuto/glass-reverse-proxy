module Helpers exposing (..)


-- HELPERS


import Html exposing (Html)
import Time exposing (Posix, Zone, toHour, toMillis, toMinute, toSecond)

formatTime : Zone -> Posix -> String
formatTime zone time =
    let
        hour = String.fromInt (toHour zone time)
            |> String.padLeft 2 '0'
        minute = String.fromInt (toMinute zone time)
            |> String.padLeft 2 '0'
        second = String.fromInt (toSecond zone time)
            |> String.padLeft 2 '0'
        millis = String.fromInt (toMillis zone time)
            |> String.padLeft 3 '0'
    in
    hour ++ ":" ++ minute ++ ":" ++ second ++ "." ++ millis


px : Int -> String
px size =
    (String.fromInt size) ++ "px"


urlShorten : String -> String
urlShorten url =
    let
        lastIndexOfSlash =
            case (String.indices "/" url |> List.reverse |> List.head) of
                Just idx ->
                    idx + 1
                Nothing ->
                    0
        indexOfQuestionMark =
            case (String.indices "?" url |> List.reverse |> List.head) of
                Just idx ->
                    idx
                Nothing ->
                    -1
    in
        String.slice lastIndexOfSlash indexOfQuestionMark url


wrapElement : (List (Html msg) -> Html msg) -> Html msg -> Html msg
wrapElement container element =
    container [ element ]
