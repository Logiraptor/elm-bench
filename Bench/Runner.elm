module Bench.Runner exposing (app, Example, Suite)

{-|
Runner makes it easier to run sets of benchmarks with the Bench
package.

#Core API
@docs app, Example, Suite

-}

import Bench
import Html
import Html.App as App
import Process
import Time
import Task


{-|
Example is a named Bench.Action. The name is rendered in the final result ui.

-}
type alias Example a =
    ( String, Bench.Action a )


{-|
Suie defines a set of examples to run in the app.
-}
type alias Suite a =
    List (Example a)


type alias Model =
    { benchmarks : List ( String, Bench.Timing ) }


type Msg a
    = BenchmarkDone String Bench.Timing
    | Start (Suite a)


{-|
app creates a program which runs the provided benchmarks and renders the result

    Runner.app [("Old Implementation", (Bench.repeat testfunc)), ("New Implementation", (Bench.repeat testfunc2))]

-}
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
