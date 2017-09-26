module Tests.Tape exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (Fuzzer, int, intRange)
import Random
import Brainfuck.Tape exposing (withOptions, defaultOptions, empty, get, set, right, left, incr, decr)


emptySize : Test
emptySize =
    test "empty tape should contain zero" <|
        \() ->
            Expect.equal
                (get empty)
                0


setGet : Test
setGet =
    fuzz (intRange 0 255) "get should return what was set" <|
        \i ->
            Expect.equal
                (empty |> set i |> get)
                i


tapeOverflow : Test
tapeOverflow =
    test "tape should overflow if moved right too many times" <|
        \() ->
            let
                tape =
                    withOptions { defaultOptions | maxSize = 1 }
            in
                case right tape of
                    Just _ ->
                        Expect.fail "A move to right should have failed with a tape bounded to size 1"

                    Nothing ->
                        Expect.pass


tapeUnderflow : Test
tapeUnderflow =
    test "tape should underflow if moved left from first cell" <|
        \() ->
            Expect.equal
                (left empty)
                Nothing


increment : Test
increment =
    fuzz (intRange 0 254) "incr should increment the current cell" <|
        \i ->
            Expect.equal
                (empty |> set i |> incr |> get)
                (i + 1)


decrement : Test
decrement =
    fuzz (intRange 1 255) "decr should increment the current cell" <|
        \i ->
            Expect.equal
                (empty |> set i |> decr |> get)
                (i - 1)


cellOverflow : Test
cellOverflow =
    fuzz (intRange 1 (Random.maxInt - 1)) "cell should overflow" <|
        \max ->
            let
                tape =
                    withOptions { defaultOptions | minValue = 0, maxValue = max }
            in
                Expect.equal
                    (tape |> set max |> incr |> get)
                    0


cellUnderflow : Test
cellUnderflow =
    fuzz (intRange 0 (Random.maxInt - 1)) "cell should underflow" <|
        \min ->
            let
                tape =
                    withOptions { defaultOptions | minValue = min, maxValue = Random.maxInt }
            in
                Expect.equal
                    (tape |> set min |> decr |> get)
                    Random.maxInt
