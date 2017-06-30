module Tests exposing (..)

import Char
import Test exposing (..)
import Expect
import Fuzz exposing (Fuzzer, list, intRange)
import String
import Brainfuck.Eval as Eval


all : Test
all =
    describe "Brainfuck test suite"
        [ describe "Eval" eval ]


eval : List Test
eval =
    let
        helloWorld =
            "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

        cat =
            ",[.[-],]"

        bubbleSort =
            ">>,[>>,]<<[[<<]>>>>[<<[>+<<+>-]>>[>+<<<<[->]>[<]>>-]<<<[[-]>>[>+<-]>>[<<<+>>>-]]>>[[<+>-]>>]<]<<[>>+<<-]<<]>>>>[.>>]"
    in
        [ test "hello world" <|
            \() ->
                Expect.equal
                    (Eval.eval helloWorld [] |> Result.map bytes2str)
                    (Ok "Hello World!\n")
        , fuzz (limitedList 50 (intRange 1 255)) "cat" <|
            \stdin ->
                -- The cat program should output its input as-is
                Expect.equal
                    (Eval.eval cat stdin)
                    (Ok stdin)
        , fuzz (limitedList 8 (intRange 1 255)) "bubble sort" <|
            \stdin ->
                -- Bubble sort should sort its input
                Expect.equal
                    (Eval.eval bubbleSort stdin)
                    (Ok <| List.sort stdin)
        ]



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
