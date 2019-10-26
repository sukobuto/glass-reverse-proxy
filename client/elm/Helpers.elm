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


posixDiff : Posix -> Posix -> Int
posixDiff from to =
    (Time.posixToMillis to) - (Time.posixToMillis from)


px : Int -> String
px size =
    (String.fromInt size) ++ "px"


trimWithMarks : String -> String -> String -> String
trimWithMarks start end text =
    let
        startIdx =
            case start of
                "" ->
                    0
                _ ->
                    String.indices start text
                    |> List.reverse
                    |> List.head
                    |> Maybe.map (\idx -> idx + 1)
                    |> Maybe.withDefault 0
        endIdx =
            case end of
                "" ->
                    Nothing
                _ ->
                    String.indices end text
                    |> List.reverse
                    |> List.head
    in
    case endIdx of
        Just idx ->
            String.slice startIdx idx text

        Nothing ->
            String.dropLeft startIdx text


wrapWith : (List (Html msg) -> Html msg) -> Html msg -> Html msg
wrapWith container element =
    container [ element ]
