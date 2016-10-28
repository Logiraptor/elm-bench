module Runner exposing (..)

import Bench
import Html
import Html.App as App
import Process
import Time
import Task


type alias Example a =
    ( String, Bench.Action a )


type alias Suite a =
    List (Example a)


type alias Model =
    { benchmarks : List ( String, Bench.Timing ) }


type Msg a
    = BenchmarkDone String Bench.Timing
    | Start (Suite a)


app : Suite a -> Program Never
app suite =
    App.program
        { init =
            ( { benchmarks = [] }
            , Task.perform (Start) (Start) (Task.andThen (Process.sleep Time.millisecond) (\() -> Task.succeed suite))
            )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


runBenchmark : ( String, Bench.Action a ) -> Cmd (Msg a)
runBenchmark ( name, action ) =
    Bench.benchmark (BenchmarkDone name) action


view : Model -> Html.Html (Msg a)
view model =
    let
        viewTiming ( name, timing ) =
            Html.div []
                [ Html.h2 [] [ Html.text name ]
                , Html.text (toString timing)
                ]
    in
        Html.div []
            [ Html.h1 [] [ Html.text "Results Will Appear Below" ]
            , Html.div [] (List.map viewTiming model.benchmarks)
            ]


update : Msg a -> Model -> ( Model, Cmd (Msg a) )
update msg model =
    case msg of
        BenchmarkDone name timing ->
            ( { model | benchmarks = ( name, timing ) :: model.benchmarks }, Cmd.none )

        Start s ->
            ( model, Cmd.batch (List.map runBenchmark s) )


subscriptions : Model -> Sub (Msg a)
subscriptions model =
    Sub.none
