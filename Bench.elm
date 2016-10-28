module Bench exposing (benchmark, repeat, Timing, Action)

{-|

The Bench API makes it easy to profile elm functions. Because time is a side effect,
this package produces commands that can be run in order to get timing information.

#Core API
@docs Timing, Action, benchmark

#Helpers
@docs repeat

-}

import Task
import Time


{-|
Action represents some computation to be performed N times. This is the code that you want to profile.
-}
type alias Action a =
    Int -> a


{-|
Timing represents the result of a benchmark run.
-}
type alias Timing =
    { operationsPerSecond : Float
    , totalCalls : Int
    , timePerOp : Time.Time
    }


{-|
benchmark takes an Action and executes it with different values of N until a reliable timing can be determined.
Your app will receive a message with the Timing information when the benchmark is complete.

    type Msg =
        TimingReceived Bench.Timing

    runBenchmark : Cmd Msg
    runBenchmark =
        Bench.benchmark TimingReceived (\n -> List.sum (List.repeat n [1]))
-}
benchmark : (Timing -> msg) -> Action a -> Cmd msg
benchmark toMsg f =
    Task.perform toMsg toMsg (tryUntilAccurate f)


goalTime : Time.Time
goalTime =
    Time.second


tryUntilAccurate : Action a -> Task.Task x Timing
tryUntilAccurate f =
    let
        innerTry : Int -> Task.Task x Timing
        innerTry n =
            Task.andThen
                (benchTask n f)
                (\r ->
                    if n >= 1000000000 || r > goalTime then
                        Task.succeed { operationsPerSecond = ((toFloat n) / (Time.inSeconds r)), totalCalls = n, timePerOp = r / (toFloat n) }
                    else
                        let
                            newCount =
                                nextIterationCount r n
                        in
                            innerTry newCount
                )
    in
        innerTry 1


benchTask : Int -> Action a -> Task.Task x Time.Time
benchTask n f =
    Task.map2 (\start end -> end - start) (timeTask n f) Time.now


timeTask : Int -> Action a -> Task.Task x Time.Time
timeTask n f =
    Task.andThen Time.now
        (\start ->
            let
                _ =
                    f n
            in
                Task.succeed start
        )


{-|
repeat makes it easy to turn any function into an Action.

    repeat (\() -> "A" + "B")
-}
repeat : (() -> a) -> Action ()
repeat f n =
    if n <= 0 then
        ()
    else
        let
            _ =
                f ()
        in
            repeat f (n - 1)



-- The following is inspired by the Go standard library benchmarking tools


nextIterationCount : Time.Time -> Int -> Int
nextIterationCount t n =
    let
        timePerOp =
            t / (toFloat n)

        estimatedCount =
            (goalTime / timePerOp) * 1.2

        roundedCount =
            estimatedCount |> round |> roundUp

        clampedTotal =
            roundedCount
                |> min (n * 100)
                |> min 1000000000
    in
        if clampedTotal <= n then
            n + 1
        else
            clampedTotal


roundDown10 : Int -> Int
roundDown10 n =
    n
        |> toFloat
        |> logBase 10
        |> floor
        |> (^) 10


roundUp : Int -> Int
roundUp n =
    let
        base =
            roundDown10 n
    in
        if n <= base then
            base
        else if n <= 2 * base then
            2 * base
        else if n <= 3 * base then
            3 * base
        else if n <= 4 * base then
            4 * base
        else if n <= 5 * base then
            5 * base
        else
            10 * base
