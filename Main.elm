module Main exposing (..)

import Http
import Json.Decode as Decode
import Html exposing (text)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Plot exposing (viewBarsCustom, groups, hintGroup, Point, Bars, defaultBarsPlotCustomizations, flyingHintContainer, normalHintContainerInner)
import Dict
import Svg.Attributes exposing (fill)


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
        ("Fetching metrics from " ++ flags.url ++ "...")
        flags.url
        Nothing
    , Http.send LoadMetrics (Http.get flags.url decodeMetricList)
    )



-- GET JSON


getMetrics : String -> Http.Request (List Metric)
getMetrics url =
    Http.get url decodeMetricList


type alias Metric =
    { hourOfDay : Int
    , program : String
    , windowTitle : String
    , count : Int
    }


decodeMetricList : Decode.Decoder (List Metric)
decodeMetricList =
    Decode.list
        (Decode.map4 Metric
            (Decode.field "hour_of_day" Decode.int)
            (Decode.field "program" Decode.string)
            (Decode.field "window_title" Decode.string)
            (Decode.field "count" Decode.int)
        )


decodeMetric : Decode.Decoder Metric
decodeMetric =
    Decode.map4 Metric
        (Decode.field "hour_of_day" Decode.int)
        (Decode.field "program" Decode.string)
        (Decode.field "window_title" Decode.string)
        (Decode.field "count" Decode.int)



-- SEND REQUESTS


send : String -> Cmd Msg
send url =
    Http.send LoadMetrics (getMetrics url)



-- MODEL


type alias Model =
    { status : String
    , url : String
    , metricGroups :
        Maybe
            (List
                { hourOfDay : Int
                , programs :
                    List
                        { programName : String
                        , windowTitle : String
                        , count : Int
                        }
                }
            )
    }


origin : List { x : Float, y : Float }
origin =
    [ { x = 0
      , y = 0
      }
    ]



-- UPDATE


type Msg
    = LoadMetrics (Result Http.Error (List Metric))


toMetricGroups : List Metric -> List { hourOfDay : Int, programs : List { programName : String, windowTitle : String, count : Int } }
toMetricGroups metrics =
    List.map
        (\( hourOfDay, programs ) -> { hourOfDay = hourOfDay, programs = programs })
        (Dict.toList
            (List.foldr
                (\metric dict ->
                    case Dict.get metric.hourOfDay dict of
                        Just metricsForHour ->
                            Dict.insert
                                metric.hourOfDay
                                (({ programName = metric.program, windowTitle = metric.windowTitle, count = metric.count }) :: metricsForHour)
                                dict

                        Nothing ->
                            Dict.insert
                                metric.hourOfDay
                                [ { programName = metric.program, windowTitle = metric.windowTitle, count = metric.count } ]
                                dict
                )
                Dict.empty
                metrics
            )
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadMetrics result ->
            case result of
                Ok metrics ->
                    ( { model
                        | status = (Basics.toString (List.length metrics)) ++ " metrics loaded"
                        , metricGroups = Just (toMetricGroups metrics)
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | status = "Error retrieving metrics: " ++ (Basics.toString e) }, Cmd.none )



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
                            let
                                label =
                                    if f >= 1000 then
                                        (toString (f / 1000.0)) ++ "k"
                                    else
                                        toString f
                            in
                                { position = f, view = (Plot.viewLabel [] (label)) }
                        )
                        (Plot.decentPositions summary |> Plot.remove 0)
                , flipAnchor = False
                }


bars : Model -> Bars (List (List Float)) msg
bars model =
    let
        g =
            (groups
                (List.map2
                    (hintGroup Maybe.Nothing)
                    (case model.metricGroups of
                        Nothing ->
                            []

                        Just metricGroups ->
                            List.map
                                (\group -> toString group.hourOfDay)
                                (List.filter (\g -> g.hourOfDay >= 8 && g.hourOfDay <= 18) metricGroups)
                    )
                )
            )
    in
        { g
            | styles = [ [ fill "gray" ], [ fill "lightgray" ], [ fill "#e6e9ef" ] ]
            , axis = garAxis
        }


view : Model -> Html Msg
view model =
    case model.metricGroups of
        Nothing ->
            p [] [ text model.status ]

        Just metricGroups ->
            let
                c =
                    defaultBarsPlotCustomizations
            in
                viewBarsCustom
                    { c
                        | height = 150
                        , width = 500
                        , margin = { top = 20, right = 20, bottom = 20, left = 20 }
                        , attributes = [ Svg.Attributes.fontSize "10px", Svg.Attributes.viewBox "-10 0 500 150" ]
                    }
                    (Debug.log "bars" (bars model))
                    (Debug.log
                        "data"
                        (List.map
                            (\group ->
                                List.map
                                    (\program -> toFloat program.count)
                                    group.programs
                            )
                            (List.filter (\g -> g.hourOfDay >= 8 && g.hourOfDay <= 18) metricGroups)
                        )
                    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
