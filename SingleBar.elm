module SingleBar exposing (..)

import Http
import Json.Decode as Decode
import Html exposing (text)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Plot exposing (viewBarsCustom, groups, hintGroup, garGroup, Point, Bars, defaultBarsPlotCustomizations, flyingHintContainer, normalHintContainerInner)
import Dict
import Svg.Attributes exposing (fill)
import Time exposing (Time, second, millisecond)


main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    { url : String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model
        ("Fetching metrics from " ++ flags.url)
        flags.url
        Nothing
        0
        True
    , Http.send LoadMetrics (Http.get flags.url (Decode.list decodeMetric))
    )



-- GET JSON


getMetrics : String -> Http.Request (List Metric)
getMetrics url =
    Http.get url (Decode.list decodeMetric)


type alias Metric =
    { dayOfWeek : String
    , avgMinutes : Float
    }


decodeMetric : Decode.Decoder Metric
decodeMetric =
    Decode.map2 Metric
        (Decode.field "day_of_week" Decode.string)
        (Decode.field "avg_minutes" Decode.float)



-- SEND REQUESTS


send : String -> Cmd Msg
send url =
    Http.send LoadMetrics (getMetrics url)



-- MODEL


type alias Model =
    { status : String
    , url : String
    , metrics : Maybe (List Metric)
    , numDots : Int
    , loading : Bool
    }



-- UPDATE


type Msg
    = LoadMetrics (Result Http.Error (List Metric))
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadMetrics result ->
            case result of
                Ok metrics ->
                    ( { model
                        | status = (Basics.toString (List.length metrics)) ++ " metrics loaded"
                        , loading = False
                        , metrics = Just metrics
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( { model
                        | loading = False
                        , status = "Error retrieving metrics: " ++ (Basics.toString err)
                      }
                    , Cmd.none
                    )

        Tick _ ->
            ( { model | numDots = (model.numDots + 1) % 4 }, Cmd.none )



-- VIEW


garAxis : Plot.Axis
garAxis =
    let
        l =
            Plot.simpleLabel
    in
        Plot.customAxis <|
            \summary ->
                { position = Plot.closestToZero
                , axisLine = Just (Plot.simpleLine summary)
                , ticks = List.map Plot.simpleTick (Plot.decentPositions summary |> Plot.remove 0)
                , labels =
                    List.map
                        (\f ->
                            { position = f
                            , view = (Plot.viewLabel [] (minuteToTime (truncate f)))
                            }
                        )
                        (Plot.decentPositions summary |> Plot.remove 0)
                , flipAnchor = False
                }


minuteToTime : Int -> String
minuteToTime m =
    (toString (m // 60))
        ++ ":"
        ++ (if String.length (toString (m % 60)) > 1 then
                (toString (m % 60))
            else
                "0" ++ (toString (m % 60))
           )


bars : Model -> Bars (List (List Float)) msg
bars model =
    let
        g =
            (groups
                (List.map3
                    (garGroup Maybe.Nothing)
                    (case model.metrics of
                        Nothing ->
                            []

                        Just metrics ->
                            List.map (\m -> m.dayOfWeek) metrics
                    )
                    (case model.metrics of
                        Nothing ->
                            []

                        Just metrics ->
                            List.map (\m -> [ minuteToTime (truncate m.avgMinutes) ]) metrics
                    )
                )
            )
    in
        { g
            | styles = [ [ fill "lightgray" ] ]
            , axis = garAxis
        }


view : Model -> Html Msg
view model =
    case model.metrics of
        Nothing ->
            p [ Html.Attributes.align "center" ]
                [ text (model.status ++ (String.repeat model.numDots "."))
                , Html.span [ Html.Attributes.style [ ( "color", "white" ) ] ]
                    [ text (String.repeat (3 - model.numDots) ".") ]
                ]

        Just metrics ->
            let
                c =
                    defaultBarsPlotCustomizations
            in
                viewBarsCustom
                    { c
                        | height = 150
                        , width = 500
                        , margin = { top = 20, right = 20, bottom = 20, left = 20 }
                        , attributes = [ Svg.Attributes.fontSize "10px", Svg.Attributes.viewBox "-10 0 500 175" ]
                        , toDomainLowest = Basics.max 530
                    }
                    (Debug.log "bars" (bars model))
                    (Debug.log
                        "data"
                        (List.map (\m -> [ m.avgMinutes ]) metrics)
                    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.loading then
        Time.every (500 * millisecond) Tick
    else
        Sub.none
