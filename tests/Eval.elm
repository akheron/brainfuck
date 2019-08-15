module Eval exposing (bubbleSort, bytes2str, cat, helloWorld, infiniteLoop, limitedList, str2bytes, tapeOverflow, tapeUnderflow)

import Brainfuck.Eval exposing (Error(..), eval)
import Char
import Expect
import Fuzz exposing (Fuzzer, intRange, list)
import String
import Test exposing (..)


helloWorld : Test
helloWorld =
    let
        code =
            "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
    in
    test "hello world" <|
        \() ->
            Expect.equal
                (eval code [] |> Result.map bytes2str)
                (Ok "Hello World!\n")


cat : Test
cat =
    let
        code =
            ",[.[-],]"
    in
    fuzz (limitedList 50 (intRange 1 255)) "cat should output its input as-is" <|
        \stdin ->
            Expect.equal
                (eval code stdin)
                (Ok stdin)


bubbleSort : Test
bubbleSort =
    let
        code =
            ">>,[>>,]<<[[<<]>>>>[<<[>+<<+>-]>>[>+<<<<[->]>[<]>>-]<<<[[-]>>[>+<-]>>[<<<+>>>-]]>>[[<+>-]>>]<]<<[>>+<<-]<<]>>>>[.>>]"
    in
    fuzz (limitedList 8 (intRange 1 255)) "bubble sort should sort its input" <|
        \stdin ->
            Expect.equal
                (eval code stdin)
                (Ok <| List.sort stdin)


infiniteLoop : Test
infiniteLoop =
    let
        code =
            "+[]"
    in
    test "infinite loop should fail" <|
        \() ->
            Expect.equal
                (eval code [])
                (Err InfiniteLoop)


tapeUnderflow : Test
tapeUnderflow =
    let
        code =
            "<"
    in
    test "tape should underflow" <|
        \() ->
            Expect.equal
                (eval code [])
                (Err TapeOverflow)


tapeOverflow : Test
tapeOverflow =
    let
        code =
            "+[>+]"
    in
    test "tape should overflow" <|
        \() ->
            Expect.equal
                (eval code [])
                (Err TapeOverflow)



-- Utils


bytes2str : List Int -> String
bytes2str =
    List.map Char.fromCode >> String.fromList


str2bytes : String -> List Int
str2bytes =
    String.toList >> List.map Char.toCode


{-| Fuzz a list of a limited length
-}
limitedList : Int -> Fuzzer a -> Fuzzer (List a)
limitedList maxLen =
    list >> Fuzz.map (List.take maxLen)
