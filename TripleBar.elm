module TripleBar exposing (..)

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
    { hourOfDay : Int
    , program : String
    , windowTitle : String
    , count : Int
    }


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
    , numDots : Int
    , loading : Bool
    }



-- UPDATE


type Msg
    = LoadMetrics (Result Http.Error (List Metric))
    | Tick Time


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
                        , loading = False
                        , metricGroups = Just (toMetricGroups metrics)
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



--            (List
--                { hourOfDay : Int
--                , programs :
--                    List
--                        { programName : String
--                        , windowTitle : String
--                        , count : Int
--                        }
--                }
--            )


bars : Model -> Bars (List (List Float)) msg
bars model =
    let
        g =
            (groups
                (List.map3
                    (garGroup Maybe.Nothing)
                    (case model.metricGroups of
                        Nothing ->
                            []

                        Just metricGroups ->
                            List.map
                                (\group -> toString group.hourOfDay)
                                (List.filter (\grp -> grp.hourOfDay >= 8 && grp.hourOfDay <= 18) metricGroups)
                    )
                    (case model.metricGroups of
                        Nothing ->
                            []

                        Just metricGroups ->
                            List.map
                                (\group -> List.map (.programName) group.programs)
                                (List.filter (\grp -> grp.hourOfDay >= 8 && grp.hourOfDay <= 18) metricGroups)
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
            p [ Html.Attributes.align "center" ]
                [ text (model.status ++ (String.repeat model.numDots "."))
                , Html.span [ Html.Attributes.style [ ( "color", "white" ) ] ]
                    [ text (String.repeat (3 - model.numDots) ".") ]
                ]

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
                        , attributes = [ Svg.Attributes.fontSize "10px", Svg.Attributes.viewBox "-10 -25 500 175" ]
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
    if model.loading then
        Time.every (500 * millisecond) Tick
    else
        Sub.none
