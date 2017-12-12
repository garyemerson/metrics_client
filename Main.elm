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
    ( Model ("Fetching metrics from " ++ flags.url ++ "...") flags.url Nothing, send flags.url )



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


data : List (List Float)
data =
    [ [ 1, 2 ]
    , [ 1, 3 ]
    , [ 2, 6 ]
    , [ 4, 8 ]
    ]


bars : Model -> Bars (List (List Float)) msg
bars model =
    groups
        (List.map2
            (hintGroup Maybe.Nothing)
            (case model.metricGroups of
                Nothing ->
                    []

                Just metricGroups ->
                    List.map
                        (\group -> toString group.hourOfDay)
                        metricGroups
            )
        )


view : Model -> Html Msg
view model =
    case model.metricGroups of
        Nothing ->
            p [] [ text model.status ]

        Just metricGroups ->
            viewBarsCustom
                { defaultBarsPlotCustomizations | height = 150, width = 200, margin = { top = 20, right = 20, bottom = 20, left = 20 } }
                (bars model)
                (List.map
                    (\group ->
                        List.map
                            (\program -> toFloat program.count)
                            group.programs
                    )
                    metricGroups
                 {- data -}
                )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
